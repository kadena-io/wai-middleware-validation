{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE LambdaCase      #-}

module Network.Wai.Middleware.Validation
    ( mkValidator
    , validatorMiddleware
    , Log(..)
    , ValidationException(..)
    , toApiDefinition
    )
    where

import Control.Applicative
import Control.Exception
import qualified Control.Exception.Safe as Safe
import Control.Lens hiding ((.=), lazy)
import qualified Control.Lens.Unsound as Unsound
import Control.Monad
import qualified Data.Aeson as Aeson
import Data.Align
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.CaseInsensitive(CI)
import qualified Data.CaseInsensitive as CI
import Data.Foldable
import Data.HashMap.Strict.InsOrd (InsOrdHashMap, keys)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.These
import Data.Typeable
import GHC.Exts
import Network.HTTP.Media
import Network.HTTP.Types
import qualified Network.Wai as Wai
import System.FilePath (splitDirectories)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Debug.Trace

import qualified Data.OpenApi as OA

import qualified Data.OpenApi.Schema.Generator as OA

data ErrorProvenance
    = RequestError
    | ResponseError
    | CombinedError
    deriving Show

data ValidationException
    = ValidationException [(ErrorProvenance, String)]
    deriving (Typeable)

instance Semigroup ValidationException where
    ValidationException errs <> ValidationException errs' =
        ValidationException (errs <> errs')

instance Show ValidationException where
    show (ValidationException errs) = unlines $
        [ show p <> ": " <> err
        | (p, err) <- errs
        ]

instance Exception ValidationException

type CoverageMap = M.Map FilePath (M.Map StdMethod (M.Map (Maybe Int) (M.Map MediaType Int), M.Map MediaType Int))

data Log = Log
    { logViolation :: (L.ByteString, Wai.Request) -> (L.ByteString, Wai.Response) -> ValidationException -> IO ()
    , logCoverage :: CoverageMap -> IO ()
    }

vRequestError :: String -> a
vRequestError e = throw $ ValidationException [(RequestError, e)]

vResponseError :: String -> a
vResponseError e = throw $ ValidationException [(ResponseError, e)]

data ValidatorConfiguration = ValidatorConfiguration
    { configuredPathPrefix :: !BS.ByteString
    , configuredApiDefinition :: !ApiDefinition
    , configuredLog :: !Log
    }

data ApiDefinition = ApiDefinition
    { getOpenApi :: !OA.OpenApi
    , getPathMap :: !PathMap
    } deriving (Eq, Show)

--
-- For reverse look up of path
-- https://swagger.io/specification/#path-templating-matching
--

data PathMap = PathMap
    { _pathHere :: !(Maybe FilePath)
    , _pathCapture :: !(Maybe PathMap)
    , _pathSubdirs :: !(M.Map String PathMap)
    } deriving (Show, Eq)

makeLenses ''PathMap

emptyPathMap :: PathMap
emptyPathMap = PathMap Nothing Nothing M.empty

insertPathMap :: PathMap -> FilePath -> PathMap
insertPathMap pm path = go (splitDirectories path) pm
  where
    go [] this = case _pathHere this of
        Nothing -> this { _pathHere = Just path }
        Just path' -> error $ "path conflict between " <> path <> " and " <> path'
    go (p:ps) this
        | not (null p) && head p == '{' && last p == '}'
            = this & pathCapture %~ Just . go ps . fromMaybe emptyPathMap
        | otherwise
            = this & pathSubdirs . at p %~ Just . go ps . fromMaybe emptyPathMap

makePathMap :: [FilePath] -> PathMap
makePathMap = foldl' insertPathMap emptyPathMap

lookupDefinedPath :: FilePath -> PathMap -> Maybe FilePath
lookupDefinedPath path pm = go (splitDirectories path) pm
  where
    go [] this = _pathHere this
    go (p:ps) this = asum
        [ go ps =<< M.lookup p (_pathSubdirs this)
        , go ps =<< _pathCapture this
        ]

toApiDefinition :: OA.OpenApi -> ApiDefinition
toApiDefinition openApi =
  ApiDefinition openApi pathMap
  where
    pathMap = makePathMap (keys $ openApi ^. OA.paths)

-- | Make a middleware for Request/Response validation.
mkValidator :: Log -> S8.ByteString -> OA.OpenApi -> IO Wai.Middleware
mkValidator lg pathPrefix openApi =
    validatorMiddleware mValidatorConfig
  where
    mValidatorConfig = ValidatorConfiguration pathPrefix (toApiDefinition openApi) lg

contentTypeIsJson :: MediaType -> Bool
contentTypeIsJson ty =
    mainType ty == "application" && subType ty == "json"

params :: Lens' MediaType (M.Map (CI ByteString) (CI ByteString))
params f ty =
    let
        ps = parameters ty
        mt = mainType ty
        st = subType ty
    in f ps <&> \ps' ->
        M.foldlWithKey' (\ty' pk pv -> ty' /: (CI.original pk, CI.original pv)) (CI.original mt // CI.original st) ps'

-- the default charset for application/json is utf-8 per the spec, so the MIME
-- types application/json and application/json;charset=utf-8 are identical. we
-- look up request and response body schemas by content type, so this
-- normalization lets us avoid duplicating entries for the two MIME types.  the
-- `q` parameter for a content type is only used to indicate level of preference
-- for that type in an Accept header, making it useless for us.
normalizeContentType :: MediaType -> MediaType
normalizeContentType ty =
    ty
        & params . at "charset" %~ stripCharsetUtf8
        & params . at "q" .~ Nothing
    where
    stripCharsetUtf8 (Just "utf-8") | contentTypeIsJson ty = Nothing
    stripCharsetUtf8 v = v

deref :: OA.OpenApi -> Lens' OA.Components (OA.Definitions c) -> OA.Referenced c -> c
deref openApi l c = OA.dereference (openApi ^. OA.components . l) c

catchErrors :: ValidatorConfiguration -> (L.ByteString, Wai.Request) -> (L.ByteString, Wai.Response) -> IO () -> IO ()
catchErrors vc req res act =
    act `Safe.catches`
        [ Safe.Handler $ logViolation (configuredLog vc) req res
        , Safe.Handler $ \(ex :: SomeException) -> logViolation (configuredLog vc) req res $ ValidationException [(ResponseError, "unexpected error: " <> show ex)]
        ]

also :: a -> b -> b
also a b = unsafeDupablePerformIO $ do
    a' <- try' (evaluate a)
    b' <- try' (evaluate b)
    case (a', b') of
        (Left ae, Left be) -> throwIO (ae <> be)
        (Left ae, _) -> throwIO ae
        (_, Left be) -> throwIO be
        (Right _, Right br) -> return br
    where
    try' :: IO a -> IO (Either ValidationException a)
    try' = try

orElse :: a -> a -> a
orElse a b = unsafeDupablePerformIO $ do
    a' <- try' (evaluate a)
    case a' of
        Left _ -> evaluate b
        Right ar -> return ar
    where
    try' :: IO a -> IO (Either ValidationException a)
    try' = try

orElseTraced :: a -> b -> Bool
orElseTraced a b = unsafeDupablePerformIO $ do
    a' <- try' (evaluate a)
    case a' of
        Left ae -> do
            b' <- try' (evaluate b)
            case b' of
                Left be -> throwIO (ae <> be)
                Right _ -> return True
        Right _ -> return False
    where
    try' :: IO a -> IO (Either ValidationException a)
    try' = try

assertP :: ErrorProvenance -> String -> Bool -> ()
assertP _ _ True = ()
assertP prov msg False = throw $ ValidationException [(prov, msg)]

validatorMiddleware :: ValidatorConfiguration -> IO Wai.Middleware
validatorMiddleware vc = do
    let openApi = getOpenApi $ configuredApiDefinition vc
    coverageTracker <- newIORef $ initialCoverageMap openApi
    return $ \app req sendResponse -> do
        (reqBody, newReq) <- getRequestBody req
        app newReq $ \resp -> do
            respBody <- getResponseBody resp
            catchErrors vc (reqBody, req) (respBody, resp) $ do
                let
                    method = either (\err -> vRequestError $ "non-standard HTTP method: " <> show err) id $ parseMethod $ Wai.requestMethod req
                    path = fromMaybe (vRequestError $ "path prefix not in path: " <> show (Wai.rawPathInfo req)) $
                        fmap S8.unpack $ S8.stripPrefix (configuredPathPrefix vc) (Wai.rawPathInfo req)
                    definedPath = lookupDefinedPath (S8.unpack $ Wai.rawPathInfo req) $ getPathMap (configuredApiDefinition vc)
                    pathItem = fromMaybe (vRequestError $ "no such path: " <> path) $
                        definedPath >>= \p ->
                            openApi ^? OA.paths . at p . _Just

                    legalMethods = [ m | m <- [minBound .. maxBound], isJust (operationForMethod m pathItem)]
                    -- it's always legal to HEAD an endpoint that supports GET, but
                    -- there is no response body expected.
                    fabricatedHeadOperation =
                        OA._pathItemGet pathItem &
                            _Just . OA.responses . Unsound.adjoin (OA.responses . traversed) (OA.default_ . _Just) %~
                                (\resp -> OA.Inline $ deref openApi OA.responses resp & OA.content . mapped . OA.schema .~ Nothing)

                    operation = fromMaybe (vRequestError $ "no such method for that path; legal methods are " <> show legalMethods) $ asum
                        [ operationForMethod method pathItem
                        , guard (method == HEAD) *> fabricatedHeadOperation
                        ]
                    reqContentType = normalizeContentType $ getContentType (Wai.requestHeaders req)
                    respContentType = normalizeContentType $ getContentType (Wai.responseHeaders resp)
                    specReqBody = deref openApi OA.requestBodies $
                        fromMaybe (vRequestError $ "no request body for that method") $
                        operation ^. OA.requestBody
                    reqSchema = fromMaybe (vRequestError $ "no schema for that request") $
                        specReqBody ^? OA.content . at reqContentType . _Just . OA.schema . _Just
                    validateReqSchema =
                        if elem method [POST, PUT] && contentTypeIsJson reqContentType && (isJust (operation ^. OA.requestBody) || not (L.null reqBody))
                        then validateJsonDocument (\e -> vRequestError ("error validating request body: " <> e)) openApi reqSchema reqBody
                        else ()
                    expectedQueryParams =
                        [ (T.encodeUtf8 $ OA._paramName dp, dp)
                        | p <- operation ^. OA.parameters
                        , let dp = deref openApi OA.parameters p
                        -- TODO: validate parameters not in the query
                        , OA._paramIn dp == OA.ParamQuery
                        ]
                    paramError k e =
                        vRequestError $ unwords [ "error validating query parameter", S8.unpack k, ":", e ]
                    validateQueryParams = let
                        checkQueryParam k v = case v of
                            This _ -> paramError k "this parameter is not specified"
                            That p ->
                                -- per https://swagger.io/docs/specification/describing-parameters/ section "Required and Optional Parameters",
                                -- parameters are not required by default
                                if fromMaybe False (OA._paramRequired p)
                                then paramError k "this parameter is required and not present"
                                else ()
                            These maybeValue param -> case maybeValue of
                                Nothing ->
                                    if fromMaybe False $ OA._paramAllowEmptyValue param then ()
                                    else paramError k $ unwords ["an empty value for this query parameter is not allowed"]
                                Just value -> let
                                    paramSchema = fromMaybe (paramError k "no schema specified for this parameter") $
                                        OA._paramSchema param
                                    validateRaw =
                                        validateJsonDocument (paramError k) openApi paramSchema (L.fromStrict value)
                                    validateQuoted =
                                        validateJsonDocument (paramError k) openApi paramSchema (L.fromStrict $ fold ["\"", value, "\""])
                                    in
                                        -- query parameters are usually not valid JSON
                                        -- because they lack quotes, so we add them in if
                                        -- necessary, though we strip them out again if both
                                        -- fail for a better error message.
                                        foldr orElse () [validateRaw, validateQuoted, validateRaw]
                        in
                            M.foldr also () $ M.mapWithKey checkQueryParam $ align (M.fromList $ Wai.queryString req) (M.fromList expectedQueryParams)
                    status = statusCode $ Wai.responseStatus resp
                    legalStatusCodes = operation ^. OA.responses . to OA._responsesResponses . to keys
                    specResp = deref openApi OA.responses $
                        fromMaybe (vResponseError $ "no response for that status code; legal status codes are " <> show legalStatusCodes) $ asum
                            [ operation ^. OA.responses . at status
                            , operation ^. OA.responses . OA.default_
                            ]
                    legalContentTypes = specResp ^. OA.content . to keys

                    content = fromMaybe (vResponseError $ "no content type for that response; legal content types are " <> show legalContentTypes) $
                        specResp ^? OA.content . at respContentType . _Just
                    schema = fromMaybe (vResponseError "no schema for that content") $
                        content ^? OA.schema . _Just
                    validateRespSchema =
                        if contentTypeIsJson respContentType && (null (specResp ^? OA.content) || not (L.null respBody))
                        then validateJsonDocument (\e -> vResponseError ("error validating response body: " <> e)) openApi schema respBody
                        else ()
                    maybeAcceptableMediaTypes =
                        fmap (normalizeContentType . fromString . S8.unpack) . S8.split ',' <$> lookup hAccept (Wai.requestHeaders req)
                    validateResponseContentTypeNegotiation = case maybeAcceptableMediaTypes of
                        Nothing -> False
                        Just acceptableMediaTypes ->
                            if null (acceptableMediaTypes `union` legalContentTypes)
                            then assertP CombinedError "server has no acceptable content types to return but there was no 406 response" (status == 406) `seq` True
                            else assertP CombinedError "server responded with an unacceptable content type" (any (\candidate -> respContentType `moreSpecificThan` candidate || respContentType == candidate) acceptableMediaTypes) `seq` False

                for_ ((,) <$> definedPath <*> (Just method `orElse` Nothing)) $ \(p, m) -> do
                    logCoverage (configuredLog vc) =<<
                        addCoverage coverageTracker p m status reqContentType respContentType
                evaluate $ or
                    [ pathItem `orElseTraced`
                        assertP CombinedError "path not found but there was no 404 response" (status == 404)
                    , operation `orElseTraced`
                        assertP CombinedError "method not found but there was no 405 response" (status == 405)
                    , validateResponseContentTypeNegotiation
                    , (validateReqSchema `also` validateQueryParams `also` validateRespSchema) `orElseTraced`
                        assertP CombinedError "invalid request body or query params but there was no 400 response" (status == 400)
                    ] `seq` ()

            sendResponse resp

initialCoverageMap :: OA.OpenApi -> CoverageMap
initialCoverageMap openApi = M.fromList
    (InsOrdHashMap.toList $ openApi ^. OA.paths) <&> \pi -> M.fromList
        [ (meth,
            ( M.fromList
                [ (status, M.fromList
                    [ (normalizeContentType mediaType, 0)
                    | (mediaType, _) <- content
                    ])
                | (status, resp) <- maybeToList ((Nothing,) <$> OA._responsesDefault resps) ++ over (mapped._1) Just (InsOrdHashMap.toList (OA._responsesResponses resps))
                , let content = InsOrdHashMap.toList $ deref openApi OA.responses resp ^. OA.content
                ]
            , M.fromList
                [ (normalizeContentType mediaType, 0)
                | Just req <- [op ^. OA.requestBody]
                , (mediaType, _) <- InsOrdHashMap.toList $ deref openApi OA.requestBodies req ^. OA.content
                ]
            ))
        | meth <- [DELETE, GET, PATCH, POST, PUT]
        , Just op <- [operationForMethod meth pi]
        , let
            resps = op ^. OA.responses
        ]

addCoverage :: IORef CoverageMap -> FilePath -> StdMethod -> Int -> MediaType -> MediaType -> IO CoverageMap
addCoverage coverageTracker path method status reqContentType respContentType =
    atomicModifyIORef' coverageTracker $ \m ->
        join (,) $ m & at path . _Just . at method . _Just %~
            (_1 %~ incResp) .
            (_2 . at reqContentType . _Just +~ 1)
    where
    orDefault m =
        case M.lookup (Just status) m of
            Nothing -> at Nothing
            Just _ -> at (Just status)
    incResp m =
        m & orDefault m . _Just . at respContentType . _Just +~ 1

getRequestBody :: Wai.Request -> IO (L.ByteString, Wai.Request)
getRequestBody req = do
    body <- Wai.strictRequestBody req
    -- The body has been consumed and needs to be refilled.
    ref <- newIORef body
    let newRequestBody = atomicModifyIORef ref (L.empty,)
    let newReq = req { Wai.requestBody = L.toStrict <$> newRequestBody }
    return (body, newReq)

getResponseBody :: Wai.Response -> IO L.ByteString
getResponseBody res = do
    let (_, _, withBody) = Wai.responseToStream res
    withBody $ \streamingBody -> do
        ref <- newIORef mempty
        streamingBody
            (\b -> atomicModifyIORef ref $ \acc -> (acc <> b, ()))
            (pure ())
        toLazyByteString <$> readIORef ref

-- internals

operationForMethod :: StdMethod -> OA.PathItem -> Maybe OA.Operation
operationForMethod DELETE = OA._pathItemDelete
operationForMethod GET = OA._pathItemGet
operationForMethod PATCH = OA._pathItemPatch
operationForMethod POST = OA._pathItemPost
operationForMethod PUT = OA._pathItemPut
operationForMethod _ = const Nothing

getContentType :: [Header] -> MediaType
getContentType headers =
    fromMaybe "application/json" $
        fromString . S8.unpack <$> lookup hContentType headers

validateJsonDocument :: (forall a. String -> a) -> OA.OpenApi -> OA.Referenced OA.Schema -> L.ByteString -> ()
validateJsonDocument err openApi bodySchema dataJson =
  case errors of
    [] -> ()
    _ -> err $ unlines errors
  where
    decoded = fromMaybe (err "The document is not valid JSON") $ Aeson.decode dataJson
    allSchemas = openApi ^. OA.components . OA.schemas
    dereferencedSchema = OA.dereference allSchemas bodySchema
    errors = map fixValidationError $ OA.validateJSON allSchemas dereferencedSchema decoded

fixValidationError :: String -> String
fixValidationError msg = T.unpack $ foldr (uncurry T.replace) (T.pack msg) replacements
  where
    replacements =
        -- replace internal type name with OpenAPI standard type name
        [ ("OpenApiString",  "string")
        , ("OpenApiNumber",  "number")
        , ("OpenApiInteger", "integer")
        , ("OpenApiBoolean", "boolean")
        ]
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
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies      #-}

module Network.Wai.Middleware.Validation
    ( mkValidator
    , validatorMiddleware
    , Log(..)
    , TopLevelError(..)
    , toApiDefinition
    , initialCoverageMap
    , CoverageMap(..)
    , EndpointCoverage(..)
    , RequestCoverage(..)
    , ResponseCoverage(..)
    )
    where

import Control.Exception
import qualified Control.Exception.Safe as Safe
import Control.Lens hiding ((.=), lazy)
import qualified Control.Lens.Unsound as Unsound
import Control.Monad
import Control.Monad.Except
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Align
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.CaseInsensitive(CI)
import qualified Data.CaseInsensitive as CI
import Data.Foldable
import Data.HashMap.Strict.InsOrd (keys)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.These
import Data.Validation
import GHC.Generics hiding (to)
import Network.HTTP.Media
import Network.HTTP.Types
import qualified Network.Wai as Wai
import System.FilePath (splitDirectories)
import System.IO.Unsafe (unsafeInterleaveIO)
import Text.Regex.TDFA

import qualified Data.OpenApi as OA

import qualified Data.OpenApi.Schema.Generator as OA

data TopLevelError
    = TopLevelError
    { requestError :: !(Maybe ([String], Status -> Bool))
    , responseError :: !(Maybe ([String], Status -> Bool))
    }

instance Show TopLevelError where
    show e = unlines $ concat
        [ ["compliance error:"]
        , showErrs "Request errors" (requestError e)
        , showErrs "Response errors" (responseError e)
        ]
        where
        showErrs _ Nothing = []
        showErrs m (Just (errs, _)) =
            [m <> ":"] ++
            (errs & (mapped . lined) %~ ("  " <>))

data MaybeStatus = StatusDefault | StatusInt !Int
    deriving (Eq, Ord, Show)

maybeStatusToText StatusDefault = "default"
maybeStatusToText (StatusInt n) = T.pack (show n)

instance Aeson.ToJSON MaybeStatus where
    toJSON = Aeson.toJSON . maybeStatusToText
instance Aeson.ToJSONKey MaybeStatus where
    toJSONKey = Aeson.toJSONKeyText maybeStatusToText

-- the shape of the CoverageMap must be populated by the initialCoverageMap.
-- attempting to add keys during operation is treated as an error.
newtype CoverageMap = CoverageMap (M.Map FilePath EndpointCoverage)
    deriving (Eq, Show, Generic, Aeson.ToJSON)
newtype EndpointCoverage = EndpointCoverage (M.Map T.Text (RequestCoverage, ResponseCoverage))
    deriving (Eq, Show, Generic, Aeson.ToJSON)
newtype RequestCoverage = RequestCoverage (M.Map MediaType Int)
    deriving (Eq, Show, Generic, Aeson.ToJSON)
newtype ResponseCoverage = ResponseCoverage (M.Map MaybeStatus (M.Map MediaType Int))
    deriving (Eq, Show, Generic, Aeson.ToJSON)
instance Wrapped CoverageMap
type instance Index CoverageMap = FilePath
type instance IxValue CoverageMap = EndpointCoverage
instance Ixed CoverageMap where ix k = _Wrapped' . ix k
instance At CoverageMap where at k = _Wrapped' . at k
instance Wrapped EndpointCoverage
type instance Index EndpointCoverage = T.Text
type instance IxValue EndpointCoverage = (RequestCoverage, ResponseCoverage)
instance Ixed EndpointCoverage where ix k = _Wrapped' . ix k
instance At EndpointCoverage where at k = _Wrapped' . at k
instance Wrapped RequestCoverage
type instance Index RequestCoverage = MediaType
type instance IxValue RequestCoverage = Int
instance Ixed RequestCoverage where ix k = _Wrapped' . ix k
instance At RequestCoverage where at k = _Wrapped' . at k
instance Wrapped ResponseCoverage
type instance Index ResponseCoverage = MaybeStatus
type instance IxValue ResponseCoverage = M.Map MediaType Int
instance Ixed ResponseCoverage where ix k = _Wrapped' . ix k
instance At ResponseCoverage where at k = _Wrapped' . at k

data Log = Log
    { logViolation :: (L.ByteString, Wai.Request) -> (L.ByteString, Wai.Response) -> TopLevelError -> IO ()
    , logCoverage :: CoverageMap -> IO ()
    }

data ValidatorConfiguration = ValidatorConfiguration
    { configuredPathMapping :: !(BS.ByteString -> Maybe (BS.ByteString, ApiDefinition))
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
mkValidator :: IORef CoverageMap -> Log -> (S8.ByteString -> Maybe (S8.ByteString, OA.OpenApi)) -> Wai.Middleware
mkValidator coverageRef lg pathfinder =
    validatorMiddleware coverageRef mValidatorConfig
  where
    mValidatorConfig = ValidatorConfiguration (over (mapped._Just._2) toApiDefinition pathfinder) lg

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
normalizeMediaType :: MediaType -> MediaType
normalizeMediaType ty =
    ty
        & params . at "charset" %~ stripCharsetUtf8
        & params . at "q" .~ Nothing
    where
    stripCharsetUtf8 (Just "utf-8") | contentTypeIsJson ty = Nothing
    stripCharsetUtf8 v = v

deref :: OA.OpenApi -> Lens' OA.Components (OA.Definitions c) -> OA.Referenced c -> c
deref openApi l c = OA.dereference (openApi ^. OA.components . l) c

catchErrors :: ValidatorConfiguration -> (L.ByteString, Wai.Request) -> (L.ByteString, Wai.Response) -> ExceptT TopLevelError IO () -> IO ()
catchErrors vc req res act = do
    err <- either id (\() -> TopLevelError Nothing Nothing) <$> runExceptT act `Safe.catch`
        \(ex :: SomeException) -> return $ Left $ TopLevelError
            { requestError = Just (["unexpected error: " <> show ex], const False)
            , responseError = Nothing
            }
    case (requestError err, responseError err) of
        -- a special case: a request compliance error that triggers the correct
        -- error response merits ignoring any response compliance error.  it's
        -- tempting to be more specific than this and ignore compliance errors
        -- whenever we have an error response code, but there are some special
        -- error cases specific to endpoints that we still (seem to) want to
        -- validate.
        (Just (_, statusRecovers), _) | statusRecovers (Wai.responseStatus (snd res)) ->
            return ()
        (Nothing, Just (_, statusRecovers)) | statusRecovers (Wai.responseStatus (snd res)) ->
            return ()
        (Nothing, Nothing) ->
            return ()
        _ ->
            logViolation (configuredLog vc) req res err

mkRequestError :: String -> (Status -> Bool) -> TopLevelError
mkRequestError msg st = TopLevelError
    { requestError = Just ([msg], st)
    , responseError = Nothing
    }

validatorMiddleware :: IORef CoverageMap -> ValidatorConfiguration -> Wai.Middleware
validatorMiddleware coverageRef vc app req sendResponse = do
    (reqBody, newReq) <- getRequestBody req
    app newReq $ \resp -> do
        respBody <- unsafeInterleaveIO $ getResponseBody resp
        catchErrors vc (reqBody, req) (respBody, resp) $ do
            method <- either (\err -> throwError $ mkRequestError ("non-standard HTTP method: " <> show err) (== methodNotAllowed405)) pure $
                parseMethod $ Wai.requestMethod req
            (S8.unpack -> path, spec) <- maybe (throwError $ mkRequestError ("path prefix not in path: " <> show (Wai.rawPathInfo req)) (== notFound404)) pure $
                configuredPathMapping vc (Wai.rawPathInfo req)
                -- listToMaybe $ mapMaybe (\(prefix,spec) -> (,spec) . S8.unpack <$> S8.stripPrefix prefix (Wai.rawPathInfo req)) (configuredPrefixSpecPairs vc)
            let openApi = getOpenApi spec
            definedPath <- maybe (throwError $ mkRequestError ("no such path: " <> path) (== notFound404)) pure $
                lookupDefinedPath path $ getPathMap spec
            pathItem <- maybe (throwError $ mkRequestError "pathItem: PathMap inaccurate, report this as a bug" (const False)) pure $
                openApi ^? OA.paths . at definedPath . _Just
            -- for OPTIONS anything goes as long as the path exists. TODO: check the contents of the response.
            when (method == OPTIONS) $
                throwError $ TopLevelError Nothing Nothing
            let
                legalMethods = [ m | m <- [minBound .. maxBound], isJust (operationForMethod m pathItem)]
                -- it's always legal to HEAD an endpoint that supports GET, but
                -- there is no response body expected.
                fabricatedHeadOperation =
                    OA._pathItemGet pathItem &
                        _Just . OA.responses . Unsound.adjoin (OA.responses . traversed) (OA.default_ . _Just) %~
                            (\schemaResp -> OA.Inline $ deref openApi OA.responses schemaResp & OA.content . mapped . OA.schema .~ Nothing)

            operation <- maybe (throwError $ mkRequestError ("no such method for that path; legal methods are " <> show legalMethods) (== methodNotAllowed405)) pure $ asum
                [ operationForMethod method pathItem
                , guard (method == HEAD) *> fabricatedHeadOperation
                ]
            let
                reqContentType = normalizeMediaType $ getContentType (Wai.requestHeaders req)
                respContentType = normalizeMediaType $ getContentType (Wai.responseHeaders resp)
                status = statusCode $ Wai.responseStatus resp
                legalStatusCodes = operation ^. OA.responses . to OA._responsesResponses . to keys
                maybeAcceptableMediaTypes =
                    fmap (normalizeMediaType . fromString . S8.unpack) . S8.split ',' <$> lookup hAccept (Wai.requestHeaders req)
                validateRequest =
                    let
                        validateReqSchema = fromEither $ do
                            if elem method [POST, PUT] && contentTypeIsJson reqContentType && (isJust (operation ^. OA.requestBody) || not (L.null reqBody))
                            then do
                                specReqBody <- fmap (deref openApi OA.requestBodies) $
                                    maybe (Left (["no specified request body for that method"], False)) Right $
                                        operation ^. OA.requestBody
                                reqSchema <- maybe (Left (["no schema for that request"], False)) Right $
                                    specReqBody ^? OA.content . at reqContentType . _Just . OA.schema . _Just
                                over _Left (\e -> (["error validating request body: " <> e], True)) $ validateJsonDocument openApi reqSchema reqBody
                            else Right ()
                        expectedQueryParams =
                            [ (T.encodeUtf8 $ OA._paramName dp, dp)
                            | p <- operation ^. OA.parameters
                            , let dp = deref openApi OA.parameters p
                            -- TODO: validate parameters not in the query
                            , OA._paramIn dp == OA.ParamQuery
                            ]
                        paramError k e =
                            [ unwords [ "error validating query parameter", S8.unpack k, ":", e ] ]
                        validateQueryParams = let
                            checkQueryParam k v = case v of
                                This _ -> Failure $ paramError k "this parameter is not specified"
                                That p ->
                                    -- per https://swagger.io/docs/specification/describing-parameters/ section "Required and Optional Parameters",
                                    -- parameters are not required by default
                                    if fromMaybe False (OA._paramRequired p)
                                    then Failure $ paramError k "this parameter is required and not present"
                                    else Success ()
                                These maybeValue param -> case maybeValue of
                                    Nothing ->
                                        if fromMaybe False $ OA._paramAllowEmptyValue param
                                        then Success ()
                                        else Failure $ paramError k $ unwords ["an empty value for this query parameter is not allowed"]
                                    Just value -> fromEither $ do
                                        paramSchema <- maybe (Left $ paramError k "no schema specified for this parameter") Right $
                                            OA._paramSchema param
                                        let
                                            validateRaw =
                                                validateJsonDocument openApi paramSchema (L.fromStrict value)
                                            validateQuoted =
                                                validateJsonDocument openApi paramSchema (L.fromStrict $ fold ["\"", value, "\""])
                                        -- query parameters are usually not valid JSON
                                        -- because they lack quotes, so we add them in if
                                        -- necessary, though we strip them out again if both
                                        -- fail for a better error message.
                                        over _Left (paramError k) $
                                            case validateRaw of
                                                Left x -> case validateQuoted of
                                                    Left _ -> Left x
                                                    r -> r
                                                r -> r
                            in
                                traverse_ snd $ M.toList $ M.mapWithKey checkQueryParam $ align (M.fromList $ Wai.queryString req) (M.fromList expectedQueryParams)
                        validateRequestIsReasonable = case maybeAcceptableMediaTypes of
                            Nothing -> Right ()
                            Just [] -> Right ()
                            Just acceptableMediaTypes ->
                                let
                                    -- we can't perfectly detect correct content type negotiation
                                    -- (specifically when it's appropriate to return 406) because we
                                    -- don't know which status code would have been appropriate to
                                    -- return if not for a failed content type negotiation, and that
                                    -- status code would tell us which content types were legal.  to
                                    -- get around that, we assume all content types belonging to
                                    -- successful status codes are valid.
                                    allSuccessfulStatusCodes = [ c | c <- legalStatusCodes, 200 <= c, c < 300 ]
                                    allLegalContentTypes = operation ^. OA.responses . foldr (\k l -> at k <> l) OA.default_ allSuccessfulStatusCodes . _Just . to (deref openApi OA.responses) . OA.content . to keys
                                in if all (\candidate -> not (any (\legal -> legal `moreSpecificThan` candidate || legal == candidate) allLegalContentTypes)) acceptableMediaTypes
                                then Left (["server has no acceptable content types to return but there was no 406 response"], (== notAcceptable406))
                                else Right ()
                        schemaParamError =
                            case (validateReqSchema, validateQueryParams) of
                                -- fatal error, unrecoverable by a 400 status
                                (Failure (bodyErrs, False), _) -> Left (bodyErrs, const False)
                                (Failure (bodyErrs, True), paramErrs) ->
                                    Left (bodyErrs <> validation id (const []) paramErrs, (== badRequest400))
                                (Success (), paramErrs) -> toEither $ over _Failure (,(== badRequest400)) paramErrs
                    in
                        schemaParamError >> validateRequestIsReasonable
                validateResponse = do
                    (deref openApi OA.responses -> specResp) <-
                        maybe (Left ("no response for that status code; legal status codes are " <> show legalStatusCodes, const False)) Right $ asum
                            [ operation ^. OA.responses . at status
                            , operation ^. OA.responses . OA.default_
                            ]
                    let
                        legalContentTypes = specResp ^. OA.content . to keys
                        validateRespSchema =
                            if contentTypeIsJson respContentType && (null (specResp ^? OA.content) || not (L.null respBody))
                            then do
                                content <- maybe (Left ("no content type for that response; legal content types are " <> show legalContentTypes, const False)) Right $
                                    specResp ^? OA.content . at respContentType . _Just
                                schema <- maybe (Left ("no schema for that content", const False)) Right $
                                    content ^? OA.schema . _Just
                                over _Left (\e -> ("error validating response body: " <> e, const False)) $ validateJsonDocument openApi schema respBody
                            else Right ()
                        validateResponseContentTypeAcceptable = case maybeAcceptableMediaTypes of
                            Nothing -> Right ()
                            Just [] -> Right ()
                            Just acceptableMediaTypes ->
                                if any (\candidate -> respContentType `moreSpecificThan` candidate || respContentType == candidate) acceptableMediaTypes
                                then Right ()
                                -- content type negotiation is irrelevant during other errors.
                                else Left ("server responded with an unacceptable content type", \s -> statusCode s >= 400 && statusCode s < 600)

                    validateResponseContentTypeAcceptable >> validateRespSchema

            liftIO $ logCoverage (configuredLog vc) =<<
                addCoverage coverageRef definedPath method status reqContentType respContentType
            throwError $ TopLevelError
                { requestError = either Just (\() -> Nothing) validateRequest
                , responseError = either (Just . over _1 (:[])) (\() -> Nothing) validateResponse
                }

        sendResponse resp

initialCoverageMap :: [OA.OpenApi] -> CoverageMap
initialCoverageMap specs =
    CoverageMap $ M.unions $
    [ M.fromList
        (InsOrdHashMap.toList $ spec ^. OA.paths) <&> \p -> EndpointCoverage (M.fromList
            [ (T.decodeUtf8 $ renderStdMethod meth,
                ( RequestCoverage $ M.fromList
                    [ (normalizeMediaType mediaType, 0)
                    | Just req <- [o ^. OA.requestBody]
                    , (mediaType, _) <- InsOrdHashMap.toList $ deref spec OA.requestBodies req ^. OA.content
                    ]
                , ResponseCoverage $ M.fromList
                    [ (status, M.fromList
                        [ (normalizeMediaType mediaType, 0)
                        | (mediaType, _) <- content
                        ])
                    | (status, resp) <- maybeToList ((StatusDefault,) <$> OA._responsesDefault resps) ++ over (mapped._1) StatusInt (InsOrdHashMap.toList (OA._responsesResponses resps))
                    , let content = InsOrdHashMap.toList $ deref spec OA.responses resp ^. OA.content
                    ]
                ))
            | meth <- [DELETE, GET, PATCH, POST, PUT]
            , Just o <- [operationForMethod meth p]
            , let
                resps = o ^. OA.responses
            ])
    | spec <- specs
    ]

addCoverage :: IORef CoverageMap -> FilePath -> StdMethod -> Int -> MediaType -> MediaType -> IO CoverageMap
addCoverage coverageTracker path method status reqContentType respContentType =
    atomicModifyIORef' coverageTracker $ \m ->
        join (,) $ m & at path . _Just . at (T.decodeUtf8 $ renderStdMethod method) . _Just %~
            (_1 . at reqContentType . _Just +~ 1) .
            (_2 %~ incResp)
    where
    orDefault m =
        case m ^? at (StatusInt status) of
            Nothing -> at StatusDefault
            Just _ -> at (StatusInt status)
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

validateJsonDocument :: OA.OpenApi -> OA.Referenced OA.Schema -> L.ByteString -> Either String ()
validateJsonDocument openApi bodySchema dataJson = do
    decoded <- maybe (Left "The document is not valid JSON") Right $ Aeson.decode dataJson
    let errors = map fixValidationError $ OA.validateJSONWithPatternChecker (flip (=~)) allSchemas dereferencedSchema decoded
    case errors of
        [] -> Right ()
        _ -> Left $ unlines errors
  where
    allSchemas = openApi ^. OA.components . OA.schemas
    dereferencedSchema = OA.dereference allSchemas bodySchema

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
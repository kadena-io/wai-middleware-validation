{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs      #-}

module Network.Wai.Middleware.Validation
    ( mkValidator
    , requestValidator
    , responseValidator
    , Log(..)
    , validateRequestBody
    , validateResponseBody
    , toApiDefinition
    )
    where

import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Builder                    (toLazyByteString)
import qualified Data.ByteString.Char8                      as S8
import qualified Data.ByteString.Lazy                       as L
import Data.Foldable
import Data.HashMap.Strict.InsOrd (InsOrdHashMap, keys)
import Data.IORef                                 (atomicModifyIORef, newIORef, readIORef)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Data.Typeable
import Network.HTTP.Media.MediaType
import Network.HTTP.Types
import qualified Network.Wai as Wai
import System.FilePath (splitDirectories)

import Data.OpenApi
  ( HttpStatusCode, OpenApi, Operation, PathItem
  , Referenced, Schema, components, content, default_
  , paths, requestBodies, requestBody, responses
  , schema, schemas, validateJSON, _pathItemDelete
  , _pathItemGet, _pathItemPatch, _pathItemPost
  , _pathItemPut
  )
import Data.OpenApi.Schema.Generator (dereference)

data ErrorProvenance
    = RequestError
    | ResponseError

data ValidationException
    = ValidationException !ErrorProvenance String
    deriving (Typeable)

instance Show ValidationException where
    show (ValidationException RequestError msg) = "Request invalid:\n" <> msg
    show (ValidationException ResponseError msg) = "Response invalid:\n" <> msg

instance Exception ValidationException

newtype Log = Log { runLog :: Wai.Request -> Maybe Wai.Response -> ValidationException -> IO () }

vError :: ErrorProvenance -> String -> a
vError p m = throw $ ValidationException p m

data ValidatorConfiguration = ValidatorConfiguration
    { configuredPathPrefix :: !BS.ByteString
    , configuredApiDefinition :: !ApiDefinition
    , configuredLog :: !Log
    }

data ApiDefinition = ApiDefinition
    { getOpenApi :: !OpenApi
    , getPathMap :: !PathMap
    } deriving (Eq, Show)

toApiDefinition :: OpenApi -> ApiDefinition
toApiDefinition openApi =
  ApiDefinition openApi pathMap
  where
    pathMap = makePathMap (keys $ openApi ^. paths)

--
-- For reverse look up of path
-- https://swagger.io/specification/#path-templating-matching
--

data TemplatedPathComponent = Exact FilePath | ParameterValue
    deriving (Eq, Ord, Show)

toTemplatedPathComponent :: FilePath -> TemplatedPathComponent
toTemplatedPathComponent s
    | not (null s) && head s == '{' && last s == '}' = ParameterValue
    | otherwise = Exact s

toTemplatedPath :: FilePath -> [TemplatedPathComponent]
toTemplatedPath p = toTemplatedPathComponent <$> splitDirectories p

type PathMap = M.Map [TemplatedPathComponent] FilePath

makePathMap :: [FilePath] -> PathMap
makePathMap ps = M.fromList $ zip (map toTemplatedPath ps) ps

lookupDefinedPath :: FilePath -> PathMap -> Maybe FilePath
lookupDefinedPath realPath = M.lookup (toTemplatedPath realPath)

-- TODO: check that the response content type is an accepted content type in the request (or something)

-- | Make a middleware for Request/Response validation.
mkValidator :: Log -> S8.ByteString -> OpenApi -> Wai.Middleware
mkValidator log pathPrefix openApi =
    responseValidator mValidatorConfig . requestValidator mValidatorConfig
  where
    mValidatorConfig = ValidatorConfiguration pathPrefix (toApiDefinition openApi) log

requestValidator :: ValidatorConfiguration -> Wai.Middleware
requestValidator vc app req sendResponse = do
    let
        eMethod = parseMethod $ Wai.requestMethod req
        Just path = S8.unpack <$> S8.stripPrefix (configuredPathPrefix vc) (Wai.rawPathInfo req)
        mContentType = getContentType (Wai.requestHeaders req)

    (body, newReq) <- getRequestBody req

    handle (runLog (configuredLog vc) req Nothing) $ evaluate $
        case (mContentType, eMethod) of
            (Nothing, _) ->
                vError ResponseError "no content type"
            (_, Left err) ->
                vError ResponseError $ "error parsing HTTP method: " <> S8.unpack err
            (Just contentType, Right method) -> do
                let
                    schema = fromMaybe (vError ResponseError "no such path/contentType/method combination") $
                        requestSchema (configuredApiDefinition vc) path contentType method
                validateJsonDocument (vError RequestError) (configuredApiDefinition vc) schema body

    app req sendResponse

responseValidator :: ValidatorConfiguration -> Wai.Middleware
responseValidator vc app req sendResponse = app req $ \res -> do
    let
        status = Wai.responseStatus res
        eMethod = parseMethod $ Wai.requestMethod req
        Just path = S8.unpack <$> S8.stripPrefix (configuredPathPrefix vc) (Wai.rawPathInfo req)
        mContentType = getContentType (Wai.responseHeaders res)
        statusCode' = statusCode status

    putStrLn ">>> [Response]"
    putStrLn $ ">>> Method: " ++ show eMethod
    putStrLn $ ">>> Path: " ++ path
    putStrLn $ ">>> Status: " ++ show statusCode'

    body <- getResponseBody res

    handle (runLog (configuredLog vc) req (Just res)) $ evaluate $
        case (mContentType, eMethod) of
            (Nothing, _) -> do
                vError ResponseError "no content type"
            (_, Left err) ->
                vError ResponseError $ "error parsing HTTP method: " <> S8.unpack err
            (Just contentType, Right method) -> do
                let
                    schema = fromMaybe (vError RequestError "no such path/contentType/method combination") $
                        responseSchema (configuredApiDefinition vc) path contentType method statusCode'
                validateJsonDocument (vError ResponseError) (configuredApiDefinition vc) schema body
    sendResponse res

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

--
-- for non-middleware use
--

validateRequestBody :: RequestHeaders -> StdMethod -> FilePath -> OpenApi -> L.ByteString -> ()
validateRequestBody rhs method path (toApiDefinition -> apiDef) body =
    case getContentType rhs of
        Nothing -> vError RequestError "no content type"
        Just contentType -> case requestSchema apiDef path contentType method of
            Nothing -> vError RequestError "Schema not found"
            Just bodySchema -> validateJsonDocument (vError RequestError) apiDef bodySchema body

validateResponseBody :: ResponseHeaders -> StdMethod -> FilePath -> Int -> OpenApi -> L.ByteString -> ()
validateResponseBody rhs method path statusCode' (toApiDefinition -> apiDef) body =
    case getContentType rhs of
        Nothing -> vError ResponseError "no content type"
        Just contentType -> case responseSchema apiDef path contentType method statusCode' of
            Nothing         -> vError ResponseError "Schema not found"
            Just bodySchema -> validateJsonDocument (vError ResponseError) apiDef bodySchema body

-- internals

operationForMethod :: StdMethod -> PathItem -> Maybe Operation
operationForMethod DELETE = _pathItemDelete
operationForMethod GET = _pathItemGet
operationForMethod PATCH = _pathItemPatch
operationForMethod POST = _pathItemPost
operationForMethod PUT = _pathItemPut
operationForMethod _ = const Nothing

operationResponseSchema :: Int -> MediaType -> OpenApi -> Operation -> Maybe (Referenced Schema)
operationResponseSchema statusCode contentType openApi operation = do
    resps <- operation ^? responses
    resp <- asum
        [ resps ^? at statusCode . _Just
        , resps ^? default_ . _Just
        ]
    deref <- dereference <$> (openApi ^? components . responses) <*> pure resp
    deref ^? content . at contentType . _Just . schema . _Just

operationRequestSchema :: MediaType -> OpenApi -> Operation -> Maybe (Referenced Schema)
operationRequestSchema contentType openApi operation = do
    req <- operation ^? requestBody
    deref <- dereference <$> (openApi ^? components . requestBodies) <*> req
    deref ^? content . at contentType . _Just . schema . _Just

getContentType headers = fromString . S8.unpack <$> lookup hContentType headers

operationForPath apiDef realPath method = getPathItem apiDef realPath >>= operationForMethod method

requestSchema apiDef realPath contentType method =
    operationForPath apiDef realPath method >>=
        operationRequestSchema contentType (getOpenApi apiDef)

responseSchema apiDef realPath contentType method statusCode =
    operationForPath apiDef realPath method >>=
        operationResponseSchema statusCode contentType (getOpenApi apiDef)

getPathItem :: ApiDefinition -> FilePath -> Maybe PathItem
getPathItem apiDef realPath = do
    definedPath <- lookupDefinedPath realPath $ getPathMap apiDef
    getOpenApi apiDef ^? paths . at definedPath . _Just

validateJsonDocument :: (forall a. String -> a) -> ApiDefinition -> Referenced Schema -> L.ByteString -> ()
validateJsonDocument err apiDef bodySchema dataJson =
  case errors of
    [] -> ()
    _ -> err $ unlines errors
  where
    decoded = fromMaybe (err "The document is not valid JSON") $ Aeson.decode dataJson
    openApi = getOpenApi apiDef
    allSchemas = fromMaybe (err "Schema objects are not defined in the OpenAPI spec") $ openApi ^? components . schemas
    dereferencedSchema = allSchemas `dereference` bodySchema
    errors = map fixValidationError $ validateJSON allSchemas dereferencedSchema decoded

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
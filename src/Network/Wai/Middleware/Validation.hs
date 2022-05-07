{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE TemplateHaskell      #-}

module Network.Wai.Middleware.Validation
    ( mkValidator
    , requestValidator
    , responseValidator
    , Log(..)
    , ValidationException(..)
    -- , validateRequestBody
    -- , validateResponseBody
    , toApiDefinition
    )
    where

import Control.Exception
import Control.Lens hiding ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Builder                    (toLazyByteString)
import qualified Data.ByteString.Char8                      as S8
import qualified Data.ByteString.Lazy                       as L
import Data.Foldable
import Data.HashMap.Strict.InsOrd (keys)
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
import Debug.Trace

import qualified Data.OpenApi as OA

import qualified Data.OpenApi.Schema.Generator as OA

data ErrorProvenance
    = RequestError
    | ResponseError

data ValidationException
    = ValidationException !ErrorProvenance String
    deriving (Typeable)

instance Show ValidationException where
    show (ValidationException RequestError msg) = "Request invalid: " <> msg
    show (ValidationException ResponseError msg) = "Response invalid: " <> msg

instance Exception ValidationException

newtype Log = Log { runLog :: Wai.Request -> Maybe Wai.Response -> ValidationException -> IO () }

vRequestError :: String -> a
vRequestError = throw . ValidationException RequestError

vResponseError :: String -> a
vResponseError = throw . ValidationException ResponseError

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
    go [] pm = case _pathHere pm of
        Nothing -> pm { _pathHere = Just path }
        Just path' -> error $ "path conflict between " <> path <> " and " <> path'
    go (p:ps) pm
        | not (null p) && head p == '{' && last p == '}'
            = pm & pathCapture %~ Just . go ps . fromMaybe emptyPathMap
        | otherwise
            = pm & pathSubdirs . at p %~ Just . go ps . fromMaybe emptyPathMap

makePathMap :: [FilePath] -> PathMap
makePathMap = foldl' insertPathMap emptyPathMap

lookupDefinedPath :: FilePath -> PathMap -> Maybe FilePath
lookupDefinedPath path pm = go (splitDirectories path) pm
  where
    go [] pm = _pathHere pm
    go (p:ps) pm = asum
        [ go ps =<< M.lookup p (_pathSubdirs pm)
        , go ps =<< _pathCapture pm
        ]

toApiDefinition :: OA.OpenApi -> ApiDefinition
toApiDefinition openApi =
  ApiDefinition openApi pathMap
  where
    pathMap = makePathMap (keys $ openApi ^. OA.paths)

-- TODO: check that the response content type is an accepted content type in the request (or something)

-- | Make a middleware for Request/Response validation.
mkValidator :: Log -> S8.ByteString -> OA.OpenApi -> Wai.Middleware
mkValidator log pathPrefix openApi =
    responseValidator mValidatorConfig . requestValidator mValidatorConfig
  where
    mValidatorConfig = ValidatorConfiguration pathPrefix (toApiDefinition openApi) log

contentTypeIsJson contentType =
    mainType contentType == "application" && subType contentType == "json"

requestValidator :: ValidatorConfiguration -> Wai.Middleware
requestValidator vc app req sendResponse = do
    (body, newReq) <- getRequestBody req

    handle (runLog (configuredLog vc) req Nothing) $ evaluate $ let
        openApi = getOpenApi $ configuredApiDefinition vc
        method = either (\err -> vResponseError $ "non-standard HTTP method: " <> show err) id $ parseMethod $ Wai.requestMethod req
        path = fromMaybe (vRequestError $ "path prefix not in path: " <> show (Wai.rawPathInfo req)) $
            fmap S8.unpack $ S8.stripPrefix (configuredPathPrefix vc) (Wai.rawPathInfo req)
        contentType = getContentType (Wai.requestHeaders req) 
        componentRequestBodies = openApi ^. OA.components . OA.requestBodies
        pathItem = fromMaybe (vRequestError $ "no such path: " <> path) $
            getPathItem (configuredApiDefinition vc) path
        operation = fromMaybe (vRequestError $ "no such method for that path") $
            operationForMethod method pathItem
        reqBody = fromMaybe (vRequestError $ "no request body for that method") $
            operation ^. OA.requestBody
        derefReqBody = OA.dereference componentRequestBodies reqBody
        reqSchema = fromMaybe (vRequestError $ "no schema for that request") $
            derefReqBody ^? OA.content . at contentType . _Just . OA.schema . _Just
        validateReqSchema =
            if elem method [POST, PUT] && contentTypeIsJson contentType then
                validateJsonDocument vRequestError (configuredApiDefinition vc) reqSchema body
            else ()
        in
            validateReqSchema

    app newReq sendResponse

responseValidator :: ValidatorConfiguration -> Wai.Middleware
responseValidator vc app req sendResponse = app req $ \res -> do
    body <- getResponseBody res

    handle (runLog (configuredLog vc) req (Just res)) $ evaluate $ let
        openApi = getOpenApi $ configuredApiDefinition vc
        status = statusCode $ Wai.responseStatus res
        method = either (\err -> vResponseError $ "non-standard HTTP method: " <> show err) id $ 
            parseMethod $ Wai.requestMethod req
        path = fromMaybe (vResponseError $ "path prefix not in path: " <> show (Wai.rawPathInfo req)) $
            fmap S8.unpack $ S8.stripPrefix (configuredPathPrefix vc) (Wai.rawPathInfo req)
        contentType = getContentType (Wai.responseHeaders res)
        pathItem = fromMaybe (vResponseError $ "no such path: " <> path) $
            getPathItem (configuredApiDefinition vc) path
        operation = fromMaybe (vResponseError $ "no such method for that path") $
            operationForMethod method pathItem
        resp = fromMaybe (vResponseError $ "no response for that status code") $ asum
            [ operation ^. OA.responses . at status 
            , operation ^. OA.responses . OA.default_ 
            ]
        derefResp = OA.dereference (openApi ^. OA.components . OA.responses) resp
        respSchema = fromMaybe (vResponseError "no schema for that response") $
            derefResp ^? OA.content . at contentType . _Just . OA.schema . _Just
        validateRespSchema =
            if contentTypeIsJson contentType
            then validateJsonDocument vResponseError (configuredApiDefinition vc) respSchema body
            else ()
        in
            validateRespSchema
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

{-
validateRequestBody :: RequestHeaders -> StdMethod -> FilePath -> OpenApi -> L.ByteString -> ()
validateRequestBody rhs method path (toApiDefinition -> apiDef) body =
    case getContentType rhs of
        Nothing -> vRequestError "no content type"
        Just contentType | bodySchema <- requestSchema apiDef path contentType method -> validateJsonDocument vRequestError apiDef bodySchema body

validateResponseBody :: ResponseHeaders -> StdMethod -> FilePath -> Int -> OpenApi -> L.ByteString -> ()
validateResponseBody rhs method path statusCode' (toApiDefinition -> apiDef) body =
    case getContentType rhs of
        Nothing -> vResponseError "no content type"
        Just contentType | bodySchema <- responseSchema apiDef path contentType method statusCode' ->
            validateJsonDocument vResponseError apiDef bodySchema body
-}

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
    fromMaybe "application/json;charset=utf-8" $ 
        fromString . S8.unpack <$> lookup hContentType headers

getPathItem :: ApiDefinition -> FilePath -> Maybe OA.PathItem
getPathItem apiDef realPath = do
    definedPath <- lookupDefinedPath realPath $ getPathMap apiDef
    getOpenApi apiDef ^? OA.paths . at definedPath . _Just

validateJsonDocument :: (forall a. String -> a) -> ApiDefinition -> OA.Referenced OA.Schema -> L.ByteString -> ()
validateJsonDocument err apiDef bodySchema dataJson =
  case errors of
    [] -> ()
    _ -> err $ unlines errors
  where
    decoded = fromMaybe (err "The document is not valid JSON") $ Aeson.decode dataJson
    openApi = getOpenApi apiDef
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
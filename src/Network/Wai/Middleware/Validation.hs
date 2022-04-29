{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Network.Wai.Middleware.Validation where

import           Control.Lens hiding ((.=))
import           Data.Aeson                                 (ToJSON, encode, object, toJSON, (.=), Value)
import           Data.ByteString.Builder                    (toLazyByteString)
import qualified Data.ByteString.Char8                      as S8
import qualified Data.ByteString.Lazy                       as L
import Data.HashMap.Strict.InsOrd (InsOrdHashMap, keys)
import           Data.IORef                                 (atomicModifyIORef, newIORef, readIORef)
import           Network.HTTP.Types                         (ResponseHeaders, StdMethod,
                                                             badRequest400, hContentType,
                                                             internalServerError500, parseMethod,
                                                             statusCode, statusIsSuccessful)
import           Network.Wai                                (Middleware, Request, Response,
                                                             rawPathInfo, requestBody,
                                                             requestMethod, responseLBS,
                                                             responseStatus, responseToStream,
                                                             strictRequestBody)

import           Network.Wai.Middleware.Validation.Internal
import           Data.OpenApi(OpenApi, paths)


data DefaultErrorJson = DefaultErrorJson
    { title  :: String
    , detail :: String
    } deriving (Show)

instance ToJSON DefaultErrorJson where
    toJSON (DefaultErrorJson t d) = object ["title" .= t, "detail" .= d]

-- | Make error string with JSON.
mkDefaultErrorJson :: String -> DefaultErrorJson
mkDefaultErrorJson = DefaultErrorJson "Validation failed"

toApiDefinition :: OpenApi -> ApiDefinition
toApiDefinition openApi =
  ApiDefinition openApi pathMap
  where
    pathMap = makePathMap (keys $ openApi ^. paths)

-- | Make a middleware for Request/Response validation.
mkValidator' :: S8.ByteString -> OpenApi -> Middleware
mkValidator' = mkValidator mkDefaultErrorJson

mkValidator :: ToJSON a => (String -> a) -> S8.ByteString -> OpenApi -> Middleware
mkValidator mkErrorJson pathPrefix openApi = mResValidator . mReqValidator
  where
    mValidatorConfig = ValidatorConfiguration pathPrefix (toApiDefinition openApi)
    mReqValidator = requestValidator mkErrorJson $ mValidatorConfig
    mResValidator = responseValidator mkErrorJson $ mValidatorConfig

-- | Make a middleware for Requestion validation.
mkRequestValidator' :: ValidatorConfiguration -> Middleware
mkRequestValidator' = mkRequestValidator mkDefaultErrorJson

mkRequestValidator :: ToJSON a => (String -> a) -> ValidatorConfiguration -> Middleware
mkRequestValidator mkErrorJson vc = requestValidator mkErrorJson vc

-- | Make a middleware for Response validation.
mkResponseValidator' :: ValidatorConfiguration -> Middleware
mkResponseValidator' = mkResponseValidator mkDefaultErrorJson

mkResponseValidator :: ToJSON a => (String -> a) -> ValidatorConfiguration -> Middleware
mkResponseValidator mkErrorJson vc = responseValidator mkErrorJson vc

requestValidator :: ToJSON a => (String -> a) -> ValidatorConfiguration -> Middleware
requestValidator mkErrorJson vc app req sendResponse = do
    let
        eMethod = parseMethod $ requestMethod req
        Just path = S8.unpack <$> S8.stripPrefix (configuredPathPrefix vc) (rawPathInfo req)
        mBodySchema = case eMethod of
            Right method -> getRequestBodySchema (configuredApiDefinition vc) method path
            _            -> Nothing

    putStrLn ">>> [Request]"
    putStrLn $ ">>> Method: " ++ show eMethod
    putStrLn $ ">>> Path: " ++ path

    case mBodySchema of
        Nothing         -> app req sendResponse
        Just bodySchema -> do
            (body, newReq) <- getRequestBody req
            putStrLn $ ">>> Body: " ++ show body

            case validateJsonDocument (configuredApiDefinition vc) bodySchema body of
                Right []   -> app newReq sendResponse
                Right errs -> respondError $ unlines errs
                Left err   -> respondError err
  where
    respondError msg = sendResponse $
        responseLBS badRequest400 [(hContentType, "application/json")] $ encode $ mkErrorJson msg

responseValidator :: ToJSON a => (String -> a) -> ValidatorConfiguration -> Middleware
responseValidator mkErrorJson vc app req sendResponse = app req $ \res -> do
    let status = responseStatus res
    -- Validate only the success response.
    if statusIsSuccessful status
        then do
            let
                eMethod = parseMethod $ requestMethod req
                Just path = S8.unpack <$> S8.stripPrefix (configuredPathPrefix vc) (rawPathInfo req)
                statusCode' = statusCode status
                mBodySchema = case eMethod of
                    Right method -> getResponseBodySchema (configuredApiDefinition vc) method path statusCode'
                    _            -> Nothing

            putStrLn ">>> [Response]"
            putStrLn $ ">>> Method: " ++ show eMethod
            putStrLn $ ">>> Path: " ++ path
            putStrLn $ ">>> Status: " ++ show statusCode'

            case mBodySchema of
                Nothing         ->
                    respondError "unrecognized path"
                    -- sendResponse res
                Just bodySchema -> do
                    body <- getResponseBody res
                    putStrLn $ ">>> Body': " ++ show body

                    case validateJsonDocument (configuredApiDefinition vc) bodySchema body of
                        Right []   -> sendResponse res
                        -- REVIEW: It may be better not to include the error details in the response.
                        -- _ -> respondError "Invalid response body"
                        Right errs -> respondError $ unlines errs
                        Left err   -> respondError err

        else sendResponse res
  where
    respondError msg = sendResponse $
        responseLBS internalServerError500 [(hContentType, "application/json")] $ encode $ mkErrorJson msg

getRequestBody :: Request -> IO (L.ByteString, Request)
getRequestBody req = do
    body <- strictRequestBody req
    -- The body has been consumed and needs to be refilled.
    ref <- newIORef body
    let newRequestBody = atomicModifyIORef ref (L.empty,)
    let newReq = req { requestBody = L.toStrict <$> newRequestBody }
    return (body, newReq)

getResponseBody :: Response -> IO L.ByteString
getResponseBody res = do
    let (_, _, withBody) = responseToStream res
    withBody $ \streamingBody -> do
        ref <- newIORef mempty
        streamingBody
            (\b -> atomicModifyIORef ref $ \acc -> (acc <> b, ()))
            (pure ())
        toLazyByteString <$> readIORef ref

responseHeaders :: ResponseHeaders
responseHeaders = [(hContentType, "application/json")]

--
-- for non-middleware use
--

validateRequestBody :: StdMethod -> FilePath -> OpenApi -> L.ByteString -> Either String [String]
validateRequestBody method path (toApiDefinition -> apiDef) body =
    case getRequestBodySchema apiDef method path of
        Nothing         -> Left "Schema not found"
        Just bodySchema -> validateJsonDocument apiDef bodySchema body

validateResponseBody :: StdMethod -> FilePath -> Int -> OpenApi -> L.ByteString -> Either String [String]
validateResponseBody method path statusCode' (toApiDefinition -> apiDef) body =
    case getResponseBodySchema apiDef method path statusCode' of
        Nothing         -> Left "Schema not found"
        Just bodySchema -> validateJsonDocument apiDef bodySchema body

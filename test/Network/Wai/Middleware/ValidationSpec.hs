{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Network.Wai.Middleware.ValidationSpec (spec) where

import Control.Exception
import Control.Monad(forM_)
import Control.Monad.IO.Class(liftIO)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Functor
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe
import Data.OpenApi(OpenApi)
import Data.String.Here                  (here)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test
import Test.Hspec

import Network.Wai.Middleware.Validation

import PredicateTransformers

testSpec :: OpenApi
testSpec = fromJust $ decode $ [here|
{
    "openapi": "3.0.0",
    "info": { "title": "validator test", "version": "1.0.0" },
    "paths": {
        "/articles": {
            "get": {
                "responses": {
                    "200": {
                        "description": "OK",
                        "content": {
                            "application/json": {
                                "schema": {
                                    "type": "array",
                                    "items": {
                                        "$ref": "#/components/schemas/Article"
                                    }
                                }
                            }
                        }
                    }
                }
            },
            "post": {
                "requestBody": {
                    "content": {
                        "application/json": {
                            "schema": {
                                "$ref": "#/components/schemas/Article"
                            }
                        },
                        "fake/type": {
                            "schema": {
                                "type": "integer"
                            }
                        }
                    }
                },
                "responses": {
                    "201": {
                        "description": "OK",
                        "content": {
                            "application/json": {
                                "schema": {
                                    "$ref": "#/components/schemas/Article"
                                }
                            },
                            "fake/type": {
                              "schema": {
                                "type": "integer"
                              }
                            }
                        }
                    }
                }
            }
        },
        "/articles/{articleId}": {
            "parameters": [
                {
                    "name": "articleId",
                    "in": "path",
                    "required": true,
                    "schema": { "type": "integer" }
                }
            ],
            "put": {
                "requestBody": {
                    "content": {
                        "application/json": {
                            "schema": {
                                "$ref": "#/components/schemas/Article"
                            }
                        }
                    }
                },
                "responses": { "default": { "description": "response example" } }
            }
        }
    },
    "components": {
        "schemas": {
            "Article": {
                "type": "object",
                "required": [ "cint", "ctxt" ],
                "properties": {
                    "cint": { "type": "integer" },
                    "ctxt": { "type": "string" }
                }
            }
        }
    }
}
|]


defaultRespHeaders = [("Content-Type", "application/json")]

validateSucceeds :: [(BS.ByteString, OpenApi)] -> (Method, FilePath, RequestHeaders, BS.ByteString) -> (Status, ResponseHeaders, BS.ByteString) -> IO ()
validateSucceeds prefixSpecMap (reqMeth, reqPath, reqHeaders, reqBody) (respStatus, respHeaders, respBody) = do
    r <- newIORef $ CoverageMap Map.empty
    void $ runSession
      (srequest $ SRequest (setPath (defaultRequest { requestMethod = reqMeth, requestHeaders = reqHeaders }) (BSC.pack reqPath)) (L.fromStrict reqBody))
      (mkValidator r (Log (\_ _ err -> print err >> stop) (\_ -> continue)) prefixSpecMap $ \_ resp -> resp $ responseLBS respStatus respHeaders (L.fromStrict respBody))
validateFails :: [(BS.ByteString, OpenApi)] -> (Method, FilePath, RequestHeaders, BS.ByteString) -> (Status, ResponseHeaders, BS.ByteString) -> IO ()
validateFails prefixSpecMap (reqMeth, reqPath, reqHeaders, reqBody) (respStatus, respHeaders, respBody) = do
    r <- newIORef $ CoverageMap Map.empty
    void $ runSession
      (srequest $ SRequest (setPath (defaultRequest { requestMethod = reqMeth, requestHeaders = reqHeaders }) (BSC.pack reqPath)) (L.fromStrict reqBody))
      (mkValidator r (Log (\_ _ _ -> stop) (\_ -> continue)) prefixSpecMap $ \_ resp -> resp $ responseLBS respStatus respHeaders (L.fromStrict respBody)) `shouldThrow` (\(_ :: PredicateFailed) -> True)
validateCoverage :: [(BS.ByteString, OpenApi)] -> (CoverageMap -> IO ()) -> (Method, FilePath, RequestHeaders, BS.ByteString) -> (Status, ResponseHeaders, BS.ByteString) -> IO ()
validateCoverage prefixSpecMap coveragePred = undefined

spec :: Spec
spec = do
    describe "Middleware" $ do
        let
            mkApp response _ respond = respond response
            validResponseBody = [here| {"cint": 1, "ctxt": "RESPONSE"} |]
            invalidResponseBody = [here| {"cint": 1} |]
            -- mkSession requestBody = srequest $ SRequest (setPath (defaultRequest {requestMethod = methodPost}) "/articles") requestBody
            validRequestBody = [here| {"cint": 1, "ctxt": "REQUEST"} |]
            invalidRequestBody = [here| {"cint": 1, "ctxt": 0} |]
            succeeds = validateSucceeds [("", testSpec)]
            fails = validateFails [("", testSpec)]
            contentTypeJson = ("Content-Type", "application/json")

        context "request and response validation" $ do

            it "do nothing if the request and response (body, content-type) is valid" $
                succeeds (methodPost, "/articles", [], validRequestBody) (created201, [contentTypeJson], validResponseBody)

            it "assume json if there is no response content type" $
                succeeds (methodPost, "/articles", [], validRequestBody) (created201, [], validResponseBody)

            it "log if the request body is invalid and response status is not 400" $
                fails (methodPost, "/articles", [], invalidRequestBody) (created201, [contentTypeJson], validResponseBody)

            it "do nothing if the request body is invalid and response status is 400" $
                succeeds (methodPost, "/articles", [], invalidRequestBody) (badRequest400, [contentTypeJson], validResponseBody)

            it "log if the response body is invalid" $
                fails (methodPost, "/articles", [], validRequestBody) (created201, [contentTypeJson], invalidResponseBody)

            it "log if there are unnecessary query parameters" $
                fails (methodPost, "/articles?doesnotexist=1", [], validRequestBody) (created201, [contentTypeJson], validResponseBody)

            it "log if the path is unknown and response status is not 404" $
                fails (methodPost, "/what", [], "") (ok200, [], "")

            it "do nothing if the path is unknown and response status is 404" $
                succeeds (methodPost, "/what", [], "") (notFound404, [], "")

            it "log if the response content type is unacceptable and response status is not 406" $
                fails (methodPost, "/articles", [("Accept", "text/plain")], validRequestBody) (ok200, [], validResponseBody)

            it "do nothing if the response content type is unacceptable and response status is not 406" $
                fails (methodPost, "/articles", [("Accept", "text/plain")], validRequestBody) (notAcceptable406, [], "")

            it "doesn't validate the request or response body unless it is json" $
                succeeds (methodPost, "/articles", [("Content-Type", "fake/type")], "madeup") (created201, [("Content-Type", "fake/type")], "madeup")

            it "always succeeds if the method is OPTIONS" $
                succeeds (methodOptions, "/whatever", [("Content-Type", "fake/type")], "madeup") (created201, [("Content-Type", "fake/type")], "madeup")
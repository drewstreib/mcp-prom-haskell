{-# LANGUAGE OverloadedStrings #-}

module MCP.TypesSpec (spec) where

import Test.Hspec
import Data.Aeson hiding (Error)
import MCP.Types

spec :: Spec
spec = do
  describe "Request parsing" $ do
    it "parses initialize request" $ do
      let json = object
            [ "jsonrpc" .= ("2.0" :: String)
            , "id" .= (1 :: Int)
            , "method" .= ("initialize" :: String)
            ]
      case decodeRequest json of
        Just req -> requestMethod req `shouldBe` Initialize
        Nothing -> expectationFailure "Failed to parse request"
    
    it "parses tools/list request" $ do
      let json = object
            [ "jsonrpc" .= ("2.0" :: String)
            , "id" .= (2 :: Int)
            , "method" .= ("tools/list" :: String)
            ]
      case decodeRequest json of
        Just req -> requestMethod req `shouldBe` ListTools
        Nothing -> expectationFailure "Failed to parse request"
    
    it "parses tools/call request" $ do
      let json = object
            [ "jsonrpc" .= ("2.0" :: String)
            , "id" .= (3 :: Int)
            , "method" .= ("tools/call" :: String)
            , "params" .= object ["name" .= ("test" :: String)]
            ]
      case decodeRequest json of
        Just req -> requestMethod req `shouldBe` CallTool
        Nothing -> expectationFailure "Failed to parse request"
  
  describe "Response encoding" $ do
    it "encodes success response" $ do
      let resp = Response
            { responseJsonrpc = "2.0"
            , responseId = Just $ Number 1
            , responseResult = Just $ object ["data" .= ("test" :: String)]
            , responseError = Nothing
            }
      let encoded = encodeResponse resp
      encoded `shouldBe` object
        [ "jsonrpc" .= ("2.0" :: String)
        , "id" .= Number 1
        , "result" .= object ["data" .= ("test" :: String)]
        ]
    
    it "encodes error response" $ do
      let resp = Response
            { responseJsonrpc = "2.0"
            , responseId = Just $ Number 1
            , responseResult = Nothing
            , responseError = Just $ MCP.Types.Error
                { errorCode = -32600
                , errorMessage = "Invalid request"
                , errorData = Nothing
                }
            }
      let encoded = encodeResponse resp
      encoded `shouldBe` object
        [ "jsonrpc" .= ("2.0" :: String)
        , "id" .= Number 1
        , "error" .= object
            [ "code" .= (-32600 :: Int)
            , "message" .= ("Invalid request" :: String)
            ]
        ]
  
  describe "Tool serialization" $ do
    it "serializes tool correctly" $ do
      let tool = Tool
            { toolName = "test_tool"
            , toolDescription = "A test tool"
            , toolInputSchema = object ["type" .= ("object" :: String)]
            }
      let encoded = toJSON tool
      encoded `shouldBe` object
        [ "name" .= ("test_tool" :: String)
        , "description" .= ("A test tool" :: String)
        , "inputSchema" .= object ["type" .= ("object" :: String)]
        ]
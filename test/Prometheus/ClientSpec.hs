{-# LANGUAGE OverloadedStrings #-}

module Prometheus.ClientSpec (spec) where

import Test.Hspec
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Prometheus.Client

spec :: Spec
spec = do
  describe "QueryResult parsing" $ do
    it "parses successful query result" $ do
      let json = object
            [ "status" .= ("success" :: String)
            , "data" .= object
                [ "resultType" .= ("vector" :: String)
                , "result" .= ([] :: [Value])
                ]
            ]
      case parseMaybe parseJSON json :: Maybe QueryResult of
        Just qr -> resultStatus qr `shouldBe` "success"
        Nothing -> expectationFailure "Failed to parse query result"
    
    it "parses error query result" $ do
      let json = object
            [ "status" .= ("error" :: String)
            , "data" .= object
                [ "errorType" .= ("bad_data" :: String)
                , "error" .= ("invalid query" :: String)
                ]
            ]
      case parseMaybe parseJSON json :: Maybe QueryResult of
        Just qr -> resultStatus qr `shouldBe` "error"
        Nothing -> expectationFailure "Failed to parse query result"
  
  describe "PrometheusClient" $ do
    it "creates client with correct base URL" $ do
      let client = newClient "http://localhost:9090"
      clientBaseUrl client `shouldBe` "http://localhost:9090"
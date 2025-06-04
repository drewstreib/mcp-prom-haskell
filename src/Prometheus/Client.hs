{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prometheus.Client
  ( PrometheusClient(..)
  , QueryResult(..)
  , query
  , queryRange
  , series
  , labelValues
  , labels
  , newClient
  ) where

import Control.Exception (try)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (statusIsSuccessful)

data PrometheusClient = PrometheusClient
  { clientBaseUrl :: String
  }

data QueryResult = QueryResult
  { resultStatus :: Text
  , resultData :: Value
  } deriving (Show)

instance FromJSON QueryResult where
  parseJSON = withObject "QueryResult" $ \v -> QueryResult
    <$> v .: "status"
    <*> v .: "data"

newClient :: String -> PrometheusClient
newClient baseUrl = PrometheusClient { clientBaseUrl = baseUrl }

query :: PrometheusClient -> Text -> Maybe UTCTime -> IO (Either String QueryResult)
query PrometheusClient{..} queryStr mTime = do
  let timeParam = case mTime of
        Nothing -> []
        Just t -> [("time", Just $ T.encodeUtf8 $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" t)]
      
  request <- parseRequest $ clientBaseUrl ++ "/api/v1/query"
  let req = setRequestQueryString
              (("query", Just $ T.encodeUtf8 queryStr) : timeParam)
              request
  
  result <- try $ httpLBS req
  case result of
    Left (e :: HttpException) -> return $ Left $ show e
    Right response ->
      if statusIsSuccessful (getResponseStatus response)
        then case eitherDecode' (getResponseBody response) of
          Left err -> return $ Left $ "Failed to parse response: " ++ err
          Right qr -> return $ Right qr
        else return $ Left $ "HTTP error: " ++ show (getResponseStatus response)

queryRange :: PrometheusClient -> Text -> UTCTime -> UTCTime -> Text -> IO (Either String QueryResult)
queryRange PrometheusClient{..} queryStr start end step = do
  let formatTime' = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
  
  request <- parseRequest $ clientBaseUrl ++ "/api/v1/query_range"
  let req = setRequestQueryString
              [ ("query", Just $ T.encodeUtf8 queryStr)
              , ("start", Just $ T.encodeUtf8 $ T.pack $ formatTime' start)
              , ("end", Just $ T.encodeUtf8 $ T.pack $ formatTime' end)
              , ("step", Just $ T.encodeUtf8 step)
              ]
              request
  
  result <- try $ httpLBS req
  case result of
    Left (e :: HttpException) -> return $ Left $ show e
    Right response ->
      if statusIsSuccessful (getResponseStatus response)
        then case eitherDecode' (getResponseBody response) of
          Left err -> return $ Left $ "Failed to parse response: " ++ err
          Right qr -> return $ Right qr
        else return $ Left $ "HTTP error: " ++ show (getResponseStatus response)

series :: PrometheusClient -> [Text] -> UTCTime -> UTCTime -> IO (Either String QueryResult)
series PrometheusClient{..} matchers start end = do
  let formatTime' = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
      matchParams = Prelude.map (\m -> ("match[]", Just $ T.encodeUtf8 m)) matchers
      timeParams = [ ("start", Just $ T.encodeUtf8 $ T.pack $ formatTime' start)
                   , ("end", Just $ T.encodeUtf8 $ T.pack $ formatTime' end)
                   ]
  
  request <- parseRequest $ clientBaseUrl ++ "/api/v1/series"
  let req = setRequestQueryString (matchParams ++ timeParams) request
  
  result <- try $ httpLBS req
  case result of
    Left (e :: HttpException) -> return $ Left $ show e
    Right response ->
      if statusIsSuccessful (getResponseStatus response)
        then case eitherDecode' (getResponseBody response) of
          Left err -> return $ Left $ "Failed to parse response: " ++ err
          Right qr -> return $ Right qr
        else return $ Left $ "HTTP error: " ++ show (getResponseStatus response)

labelValues :: PrometheusClient -> Text -> IO (Either String QueryResult)
labelValues PrometheusClient{..} labelName = do
  request <- parseRequest $ clientBaseUrl ++ "/api/v1/label/" ++ T.unpack labelName ++ "/values"
  
  result <- try $ httpLBS request
  case result of
    Left (e :: HttpException) -> return $ Left $ show e
    Right response ->
      if statusIsSuccessful (getResponseStatus response)
        then case eitherDecode' (getResponseBody response) of
          Left err -> return $ Left $ "Failed to parse response: " ++ err
          Right qr -> return $ Right qr
        else return $ Left $ "HTTP error: " ++ show (getResponseStatus response)

labels :: PrometheusClient -> IO (Either String QueryResult)
labels PrometheusClient{..} = do
  request <- parseRequest $ clientBaseUrl ++ "/api/v1/labels"
  
  result <- try $ httpLBS request
  case result of
    Left (e :: HttpException) -> return $ Left $ show e
    Right response ->
      if statusIsSuccessful (getResponseStatus response)
        then case eitherDecode' (getResponseBody response) of
          Left err -> return $ Left $ "Failed to parse response: " ++ err
          Right qr -> return $ Right qr
        else return $ Left $ "HTTP error: " ++ show (getResponseStatus response)
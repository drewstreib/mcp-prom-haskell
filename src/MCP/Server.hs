{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server
  ( runServer
  , ServerConfig(..)
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, SomeException)
import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import System.Exit (exitSuccess)
import System.IO

import MCP.Types
import Prometheus.Client

data ServerConfig = ServerConfig
  { configPrometheusUrl :: String
  }

runServer :: ServerConfig -> IO ()
runServer config = do
  -- Set UTF-8 encoding for all handles
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  
  -- Use NoBuffering to ensure immediate message delivery
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr LineBuffering  -- Keep line buffering for debug output
  
  let client = newClient (configPrometheusUrl config)
  
  let loop = do
        -- Block waiting for input, but handle EOF/closure gracefully
        inputResult <- catch (TIO.getLine >>= return . Just) handleEOF
        case inputResult of
          Nothing -> exitSuccess  -- stdin closed or EOF
          Just line -> do
            let lineBytes = TLE.encodeUtf8 (TL.fromStrict line)
            case eitherDecode' lineBytes of
              Left _err -> loop  -- Ignore parse errors, continue waiting
              Right req -> do
                response <- handleRequest client req
                let responseJson = TLE.decodeUtf8 (encode response)
                TIO.putStrLn (TL.toStrict responseJson)
                hFlush stdout
                loop
      
      handleEOF :: SomeException -> IO (Maybe Text)
      handleEOF _ = return Nothing
  
  loop

handleRequest :: PrometheusClient -> Request -> IO Response
handleRequest client Request{..} =
  case requestMethod of
    Initialize -> return $ Response
      { responseJsonrpc = "2.0"
      , responseId = requestId
      , responseResult = Just $ object
          [ "protocolVersion" .= ("2024-11-05" :: Text)
          , "capabilities" .= object
              [ "tools" .= object
                  [ "listChanged" .= False
                  ]
              ]
          , "serverInfo" .= object
              [ "name" .= ("mcp-prometheus-server" :: Text)
              , "version" .= ("0.1.0" :: Text)
              ]
          ]
      , responseError = Nothing
      }
    
    ListTools -> return $ Response
      { responseJsonrpc = "2.0"
      , responseId = requestId
      , responseResult = Just $ object
          [ "tools" .= 
              [ Tool
                  { toolName = "prometheus_query"
                  , toolDescription = "Execute a Prometheus query"
                  , toolInputSchema = object
                      [ "type" .= ("object" :: Text)
                      , "properties" .= object
                          [ "query" .= object
                              [ "type" .= ("string" :: Text)
                              , "description" .= ("PromQL query to execute" :: Text)
                              ]
                          , "time" .= object
                              [ "type" .= ("string" :: Text)
                              , "description" .= ("Evaluation timestamp (RFC3339 or Unix timestamp)" :: Text)
                              ]
                          ]
                      , "required" .= (["query"] :: [Text])
                      ]
                  }
              , Tool
                  { toolName = "prometheus_query_range"
                  , toolDescription = "Execute a Prometheus range query"
                  , toolInputSchema = object
                      [ "type" .= ("object" :: Text)
                      , "properties" .= object
                          [ "query" .= object
                              [ "type" .= ("string" :: Text)
                              , "description" .= ("PromQL query to execute" :: Text)
                              ]
                          , "start" .= object
                              [ "type" .= ("string" :: Text)
                              , "description" .= ("Start timestamp (RFC3339 or Unix timestamp)" :: Text)
                              ]
                          , "end" .= object
                              [ "type" .= ("string" :: Text)
                              , "description" .= ("End timestamp (RFC3339 or Unix timestamp)" :: Text)
                              ]
                          , "step" .= object
                              [ "type" .= ("string" :: Text)
                              , "description" .= ("Query resolution step (e.g., '15s', '1m')" :: Text)
                              ]
                          ]
                      , "required" .= (["query", "start", "end", "step"] :: [Text])
                      ]
                  }
              , Tool
                  { toolName = "prometheus_series"
                  , toolDescription = "Find series matching label matchers"
                  , toolInputSchema = object
                      [ "type" .= ("object" :: Text)
                      , "properties" .= object
                          [ "match" .= object
                              [ "type" .= ("array" :: Text)
                              , "items" .= object ["type" .= ("string" :: Text)]
                              , "description" .= ("Label matchers (e.g., 'up{job=\"node\"}')" :: Text)
                              ]
                          , "start" .= object
                              [ "type" .= ("string" :: Text)
                              , "description" .= ("Start timestamp (RFC3339)" :: Text)
                              ]
                          , "end" .= object
                              [ "type" .= ("string" :: Text)
                              , "description" .= ("End timestamp (RFC3339)" :: Text)
                              ]
                          ]
                      , "required" .= (["match", "start", "end"] :: [Text])
                      ]
                  }
              , Tool
                  { toolName = "prometheus_metrics"
                  , toolDescription = "Get all metric names"
                  , toolInputSchema = object
                      [ "type" .= ("object" :: Text)
                      , "properties" .= object []
                      ]
                  }
              , Tool
                  { toolName = "prometheus_labels"
                  , toolDescription = "Get all label names"
                  , toolInputSchema = object
                      [ "type" .= ("object" :: Text)
                      , "properties" .= object []
                      ]
                  }
              ]
          ]
      , responseError = Nothing
      }
    
    CallTool -> case requestParams of
      Nothing -> return $ errorResponse requestId "Invalid request" (-32600)
      Just params -> case parseMaybe parseJSON params of
        Nothing -> return $ errorResponse requestId "Invalid params" (-32602)
        Just toolCall -> handleToolCall client requestId toolCall
    
    Unknown method -> return $ errorResponse requestId ("Unknown method: " <> method) (-32601)

handleToolCall :: PrometheusClient -> Maybe Value -> ToolCall -> IO Response
handleToolCall client reqId ToolCall{..} =
  case toolCallName of
    "prometheus_query" -> 
      case parseMaybe parseQueryArgs toolCallArguments of
        Nothing -> return $ errorResponse reqId "Invalid query arguments" (-32602)
        Just (queryStr, mTimeStr) -> do
          let mTime = mTimeStr >>= parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . T.unpack
          result <- query client queryStr mTime
          case result of
            Left err -> return $ toolErrorResponse reqId err
            Right qr -> return $ toolSuccessResponse reqId qr
    
    "prometheus_query_range" ->
      case parseMaybe parseRangeQueryArgs toolCallArguments of
        Nothing -> return $ errorResponse reqId "Invalid range query arguments" (-32602)
        Just (queryStr, startStr, endStr, step) -> do
          let parseTime' s = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (T.unpack s)
              mStart = parseTime' startStr
              mEnd = parseTime' endStr
          case (mStart, mEnd) of
            (Just start, Just end) -> do
              result <- queryRange client queryStr start end step
              case result of
                Left err -> return $ toolErrorResponse reqId err
                Right qr -> return $ toolSuccessResponse reqId qr
            _ -> return $ errorResponse reqId "Invalid time format" (-32602)
    
    "prometheus_series" ->
      case parseMaybe parseSeriesArgs toolCallArguments of
        Nothing -> return $ errorResponse reqId "Invalid series arguments" (-32602)
        Just (matchers, startStr, endStr) -> do
          let parseTime' s = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (T.unpack s)
              mStart = parseTime' startStr
              mEnd = parseTime' endStr
          case (mStart, mEnd) of
            (Just start, Just end) -> do
              result <- series client matchers start end
              case result of
                Left err -> return $ toolErrorResponse reqId err
                Right qr -> return $ toolSuccessResponse reqId qr
            _ -> return $ errorResponse reqId "Invalid time format" (-32602)
    
    "prometheus_metrics" -> do
      result <- labelValues client "__name__"
      case result of
        Left err -> return $ toolErrorResponse reqId err
        Right qr -> return $ toolSuccessResponse reqId qr
    
    "prometheus_labels" -> do
      result <- labels client
      case result of
        Left err -> return $ toolErrorResponse reqId err
        Right qr -> return $ toolSuccessResponse reqId qr
    
    _ -> return $ errorResponse reqId ("Unknown tool: " <> toolCallName) (-32602)

parseQueryArgs :: Value -> Parser (Text, Maybe Text)
parseQueryArgs = withObject "QueryArgs" $ \v -> do
  queryStr <- v .: "query"
  mTime <- v .:? "time"
  return (queryStr, mTime)

parseRangeQueryArgs :: Value -> Parser (Text, Text, Text, Text)
parseRangeQueryArgs = withObject "RangeQueryArgs" $ \v -> do
  queryStr <- v .: "query"
  start <- v .: "start"
  end <- v .: "end"
  step <- v .: "step"
  return (queryStr, start, end, step)

parseSeriesArgs :: Value -> Parser ([Text], Text, Text)
parseSeriesArgs = withObject "SeriesArgs" $ \v -> do
  matchers <- v .: "match"
  start <- v .: "start"
  end <- v .: "end"
  return (matchers, start, end)

errorResponse :: Maybe Value -> Text -> Int -> Response
errorResponse reqId msg code = Response
  { responseJsonrpc = "2.0"
  , responseId = reqId
  , responseResult = Nothing
  , responseError = Just $ MCP.Types.Error code msg Nothing
  }

toolErrorResponse :: Maybe Value -> String -> Response
toolErrorResponse reqId err = Response
  { responseJsonrpc = "2.0"
  , responseId = reqId
  , responseResult = Just $ object
      [ "content" .= [object ["type" .= ("text" :: Text), "text" .= err]]
      , "isError" .= True
      ]
  , responseError = Nothing
  }

toolSuccessResponse :: Maybe Value -> QueryResult -> Response
toolSuccessResponse reqId QueryResult{..} = Response
  { responseJsonrpc = "2.0"
  , responseId = reqId
  , responseResult = Just $ object
      [ "content" .= [object ["type" .= ("text" :: Text), "text" .= TL.unpack (TLE.decodeUtf8 (encode resultData))]]
      , "isError" .= False
      ]
  , responseError = Nothing
  }
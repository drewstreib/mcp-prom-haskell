{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module MCP.Server
  ( runServer
  , ServerConfig(..)
  ) where

import Control.Exception (try, IOException)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, runStateT, get, modify)
import Control.Monad.IO.Class (liftIO)
import System.IO.Error (isEOFError)
import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser, parseMaybe)
import Network.HTTP.Simple (HttpException)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale, formatTime)
import System.Exit (exitSuccess)
import System.IO

import MCP.Types
import Prometheus.Client

data ServerConfig = ServerConfig
  { configPrometheusUrl :: String
  } deriving (Show, Eq)

data ServerState = ServerState
  { stateRequestCount :: Int
  , stateClient :: PrometheusClient
  }

-- Better error types for server operations  
data ServerError
  = ParseError Text Text  -- error message, raw input
  | IOError IOException
  | RequestHandlingError IOException  
  | PrometheusError Text
  deriving (Show)

-- Structured logging with timestamps
-- Use bang patterns to force strict evaluation and avoid lazy I/O issues
logInfo :: Text -> IO ()
logInfo !msg = do
  timestamp <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
  TIO.hPutStrLn stderr $ "[" <> T.pack timeStr <> "] INFO: " <> msg

logError :: Text -> IO ()
logError !msg = do
  timestamp <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
  TIO.hPutStrLn stderr $ "[" <> T.pack timeStr <> "] ERROR: " <> msg

logWarn :: Text -> IO ()
logWarn !msg = do
  timestamp <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
  TIO.hPutStrLn stderr $ "[" <> T.pack timeStr <> "] WARN: " <> msg

-- Server monad stack: Reader for config, State for connection state, IO for effects
type ServerM = ReaderT ServerConfig (StateT ServerState IO)

-- Helper to catch only HTTP-related exceptions
tryHttpRequest :: IO a -> IO (Either HttpException a)
tryHttpRequest = try

-- Run a ServerM computation
runServerM :: ServerConfig -> ServerState -> ServerM a -> IO (a, ServerState)
runServerM config state action = runStateT (runReaderT action config) state

-- Handle server errors with appropriate logging and responses  
handleServerError :: Maybe Value -> ServerError -> ServerM (Maybe Response)
handleServerError reqId err = case err of
  ParseError parseErr rawInput -> do
    liftIO $ logError $ "JSON parse error: " <> parseErr
    liftIO $ logError $ "Raw input (first 200 chars): " <> T.take 200 rawInput
    return Nothing  -- No response for parse errors
    
  IOError ex -> do
    liftIO $ logError $ "IO error: " <> T.pack (show ex)
    return $ Just $ Response
      { responseJsonrpc = "2.0"
      , responseId = reqId
      , responseResult = Nothing
      , responseError = Just $ MCP.Types.Error (-32603) "Internal server error" Nothing
      }
      
  RequestHandlingError ex -> do
    liftIO $ logError $ "Request handling failed: " <> T.pack (show ex)
    return $ Just $ Response
      { responseJsonrpc = "2.0"
      , responseId = reqId
      , responseResult = Nothing
      , responseError = Just $ MCP.Types.Error (-32603) "Internal server error" Nothing
      }
      
  PrometheusError msg -> do
    liftIO $ logError $ "Prometheus error: " <> msg
    return $ Just $ Response
      { responseJsonrpc = "2.0"
      , responseId = reqId
      , responseResult = Nothing
      , responseError = Just $ MCP.Types.Error (-32603) ("Prometheus error: " <> msg) Nothing
      }

-- Send a response and handle I/O errors
sendResponse :: Response -> ServerM ()
sendResponse response = do
  outputResult <- liftIO $ try $ do
    let responseJson = TLE.decodeUtf8 (encode response)
    TIO.putStrLn (TL.toStrict responseJson)
    hFlush stdout
  case outputResult of
    Left (ex :: IOException) -> liftIO $ logError $ "Failed to send response: " <> T.pack (show ex)
    Right _ -> return ()

-- Process a single request and send response
processRequest :: Text -> ServerM ()
processRequest line = do
  state <- get
  let requestCount = stateRequestCount state
      client = stateClient state
  
  liftIO $ logInfo $ "Received request #" <> T.pack (show requestCount) <> " (" <> T.pack (show (T.length line)) <> " chars)"
  
  result <- processRequestSafe client line
  case result of
    Left err -> do
      -- Handle error and potentially send error response
      maybeResponse <- handleServerError Nothing err
      case maybeResponse of
        Just response -> sendResponse response
        Nothing -> return ()  -- No response needed (e.g., parse errors)
    
    Right maybeResponse -> do
      case maybeResponse of
        Just response -> do
          liftIO $ logInfo $ "Sending response for request id: " <> 
                    maybe "null" (T.take 20 . T.pack . show) (responseId response)
          sendResponse response
        Nothing -> 
          liftIO $ logInfo "Received notification, no response required"
  
  -- Increment request counter regardless of outcome
  modify $ \s -> s { stateRequestCount = requestCount + 1 }

-- Safely process a request, returning either an error or optional response
processRequestSafe :: PrometheusClient -> Text -> ServerM (Either ServerError (Maybe Response))
processRequestSafe client line = do
  -- Parse JSON
  let lineBytes = TLE.encodeUtf8 (TL.fromStrict line)
  case eitherDecode' lineBytes of
    Left parseErr -> 
      return $ Left $ ParseError (T.pack parseErr) line
    
    Right req -> do
      liftIO $ logInfo $ "Parsed " <> T.pack (show (requestMethod req)) <> " request with id: " <> 
                maybe "null" (T.take 20 . T.pack . show) (requestId req)
      
      -- Check if this is a notification (no response expected)
      if requestMethod req == Notification
        then return $ Right Nothing
        else do
          -- Handle request processing with error recovery
          responseResult <- liftIO $ try (handleRequest client req)
          case responseResult of
            Left ex -> return $ Left $ RequestHandlingError ex
            Right response -> return $ Right $ Just response

-- Main connection loop - reads from stdin and processes requests
connectionLoop :: ServerM ()
connectionLoop = do
  -- Block waiting for input, but handle all errors gracefully
  inputResult <- liftIO $ try TIO.getLine
  case inputResult of
    Left ex -> do
      if isEOFError ex
        then do
          liftIO $ logInfo "Client disconnected (EOF)"
          liftIO exitSuccess
        else do
          liftIO $ logError $ "IO error reading from stdin: " <> T.pack (show ex)
          liftIO exitSuccess
    
    Right line -> do
      processRequest line
      connectionLoop

runServer :: ServerConfig -> IO ()
runServer config = do
  logInfo "Starting MCP Prometheus server..."
  
  -- Set UTF-8 encoding for all handles
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  
  -- Use NoBuffering to ensure immediate message delivery
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr LineBuffering  -- Keep line buffering for debug output
  
  logInfo $ "Connecting to Prometheus at: " <> T.pack (configPrometheusUrl config)
  let client = newClient (configPrometheusUrl config)
  
  logInfo "Server ready, waiting for MCP messages..."
  
  -- Initialize server state and run the connection loop
  let initialState = ServerState { stateRequestCount = 1, stateClient = client }
  (_, _) <- runServerM config initialState connectionLoop
  return ()

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
              , "version" .= ("1.0.1" :: Text)
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
    
    Notification -> return $ errorResponse requestId "Notifications should not reach handleRequest" (-32603)
    
    Unknown method -> return $ errorResponse requestId ("Unknown method: " <> method) (-32601)

handleToolCall :: PrometheusClient -> Maybe Value -> ToolCall -> IO Response
handleToolCall client reqId ToolCall{..} =
  case toolCallName of
    "prometheus_query" -> 
      case parseMaybe parseQueryArgs toolCallArguments of
        Nothing -> do
          logError $ "Invalid arguments for prometheus_query: " <> T.pack (show toolCallArguments)
          return $ errorResponse reqId "Invalid query arguments" (-32602)
        Just (queryStr, mTimeStr) -> do
          let mTime = mTimeStr >>= parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . T.unpack
          logInfo $ "Executing Prometheus query: " <> T.take 100 queryStr
          result <- tryHttpRequest $ query client queryStr mTime
          case result of
            Left ex -> do
              logError $ "Prometheus query failed: " <> T.pack (show ex)
              return $ toolErrorResponse reqId ("Prometheus query failed: " ++ show ex)
            Right (Left err) -> do
              logWarn $ "Prometheus query returned error: " <> T.pack err
              return $ toolErrorResponse reqId err
            Right (Right qr) -> do
              logInfo "Prometheus query completed successfully"
              return $ toolSuccessResponse reqId qr
    
    "prometheus_query_range" ->
      case parseMaybe parseRangeQueryArgs toolCallArguments of
        Nothing -> do
          logError $ "Invalid arguments for prometheus_query_range: " <> T.pack (show toolCallArguments)
          return $ errorResponse reqId "Invalid range query arguments" (-32602)
        Just (queryStr, startStr, endStr, step) -> do
          let parseTime' s = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (T.unpack s)
              mStart = parseTime' startStr
              mEnd = parseTime' endStr
          case (mStart, mEnd) of
            (Just start, Just end) -> do
              logInfo $ "Executing Prometheus range query: " <> T.take 100 queryStr
              result <- tryHttpRequest $ queryRange client queryStr start end step
              case result of
                Left ex -> do
                  logError $ "Prometheus range query failed: " <> T.pack (show ex)
                  return $ toolErrorResponse reqId ("Prometheus range query failed: " ++ show ex)
                Right (Left err) -> do
                  logWarn $ "Prometheus range query returned error: " <> T.pack err
                  return $ toolErrorResponse reqId err
                Right (Right qr) -> do
                  logInfo "Prometheus range query completed successfully"
                  return $ toolSuccessResponse reqId qr
            _ -> do
              logError $ "Invalid time format - start: " <> startStr <> ", end: " <> endStr
              return $ errorResponse reqId "Invalid time format" (-32602)
    
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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module MCP.Server
  ( runServer
  , ServerConfig(..)
  ) where

import Control.Exception (try, IOException, catch, SomeException)
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
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.ByteString.Char8 as BS
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
  | InputError Text  -- For robustness-related input errors
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

-- Constants for input protection
maxInputLength :: Int
maxInputLength = 1024 * 1024  -- 1MB limit

maxLogLength :: Int
maxLogLength = 1000  -- Truncate logs to 1000 chars

-- Safe input reading with robust UTF-8 handling and size limits
safeReadInput :: IO (Either String Text)
safeReadInput = do
  result <- try $ do
    -- Read raw bytes first to handle encoding issues
    rawBytes <- BS.hGetLine stdin
    
    -- Check input size limit
    if BS.length rawBytes > maxInputLength
      then return $ Left "Input too large (> 1MB)"
      else do
        -- Decode with lenient UTF-8 handling (replace invalid sequences)
        let decoded = TE.decodeUtf8With TEE.lenientDecode rawBytes
        -- Additional validation for decoded text
        if T.any (\c -> c < '\x20' && c /= '\n' && c /= '\t' && c /= '\r') decoded
          then return $ Left "Input contains invalid control characters"
          else return $ Right decoded
  
  case result of
    Left ex -> 
      if isEOFError ex
        then return $ Left "EOF"
        else return $ Left $ "IO error: " ++ show ex
    Right eitherText -> return eitherText

-- Safe text truncation for logging
safeTruncateForLog :: Text -> Text
safeTruncateForLog txt = 
  let truncated = T.take maxLogLength txt
      suffix = if T.length txt > maxLogLength then "..." else ""
  in truncated <> suffix

-- Sanitize text for safe logging (remove control characters except newlines/tabs)
sanitizeForLog :: Text -> Text
sanitizeForLog = T.filter (\c -> c >= ' ' || c == '\n' || c == '\t')

-- Run a ServerM computation
runServerM :: ServerConfig -> ServerState -> ServerM a -> IO (a, ServerState)
runServerM config state action = runStateT (runReaderT action config) state

-- Handle server errors with appropriate logging and responses  
handleServerError :: Maybe Value -> ServerError -> ServerM (Maybe Response)
handleServerError reqId err = case err of
  ParseError parseErr rawInput -> do
    liftIO $ logError $ "JSON parse error: " <> parseErr
    liftIO $ logError $ "Raw input (sanitized, first 200 chars): " <> 
            safeTruncateForLog (sanitizeForLog (T.take 200 rawInput))
    return Nothing  -- No response for parse errors
    
  InputError errMsg -> do
    liftIO $ logError $ "Input error: " <> errMsg
    return Nothing  -- No response for input errors
    
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

-- Send a response and handle I/O errors with size protection
sendResponse :: Response -> ServerM ()
sendResponse response = do
  outputResult <- liftIO $ try $ do
    let responseJson = TLE.decodeUtf8 (encode response)
        responseText = TL.toStrict responseJson
    -- Validate output isn't too large
    if T.length responseText > maxInputLength
      then do
        logWarn "Response too large, truncating"
        let truncated = T.take (maxInputLength - 100) responseText <> "...truncated}"
        TIO.putStrLn truncated
      else TIO.putStrLn responseText
    hFlush stdout
  
  case outputResult of
    Left (ex :: IOException) -> liftIO $ logError $ "Failed to send response: " <> T.pack (show ex)
    Right _ -> return ()
  
  -- Additional error handling for any other exceptions during output
  `catch` \(ex :: SomeException) -> 
    liftIO $ logError $ "Unexpected error sending response: " <> T.pack (show ex)

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
          let respIdStr = maybe "null" (safeTruncateForLog . T.pack . show) (responseId response)
          liftIO $ logInfo $ "Sending response for request id: " <> respIdStr
          sendResponse response
        Nothing -> 
          liftIO $ logInfo "Received notification, no response required"
  
  -- Increment request counter regardless of outcome
  modify $ \s -> s { stateRequestCount = requestCount + 1 }

-- Safely process a request, returning either an error or optional response
processRequestSafe :: PrometheusClient -> Text -> ServerM (Either ServerError (Maybe Response))
processRequestSafe client line = do
  -- Validate input isn't just whitespace or empty
  let trimmedLine = T.strip line
  if T.null trimmedLine
    then do
      liftIO $ logWarn "Received empty/whitespace-only input, ignoring"
      return $ Left $ InputError "Empty or whitespace-only input"
    else do
      -- Handle JSON parsing with comprehensive error handling
      parseResult <- liftIO $ try $ do
        let lineBytes = TLE.encodeUtf8 (TL.fromStrict trimmedLine)
        case eitherDecode' lineBytes of
          Left parseErr -> return $ Left parseErr
          Right req -> return $ Right req
      
      case parseResult of
        Left (ex :: SomeException) -> do
          liftIO $ logError $ "Exception during JSON parsing: " <> T.pack (show ex)
          return $ Left $ ParseError "Exception during parsing" trimmedLine
        
        Right (Left parseErr) -> 
          return $ Left $ ParseError (T.pack parseErr) trimmedLine
        
        Right (Right req) -> do
          let reqMethodStr = T.take 50 $ T.pack $ show $ requestMethod req
              reqIdStr = maybe "null" (safeTruncateForLog . T.pack . show) (requestId req)
          liftIO $ logInfo $ "Parsed " <> reqMethodStr <> " request with id: " <> reqIdStr
          
          -- Check if this is a notification (no response expected)
          if requestMethod req == Notification
            then return $ Right Nothing
            else do
              -- Handle request processing with comprehensive error recovery
              responseResult <- liftIO $ (try (handleRequest client req) `catch` \(ex :: SomeException) -> do
                logError $ "Unexpected exception during request handling: " <> T.pack (show ex)
                return $ Right $ Response
                  { responseJsonrpc = "2.0"
                  , responseId = requestId req
                  , responseResult = Nothing
                  , responseError = Just $ MCP.Types.Error (-32603) "Internal server error" Nothing
                  }) :: IO (Either IOException Response)
              
              case responseResult of
                Left ex -> return $ Left $ RequestHandlingError ex
                Right response -> return $ Right $ Just response

-- Main connection loop - reads from stdin and processes requests with robustness
connectionLoop :: ServerM ()
connectionLoop = do
  -- Use safe input reading with comprehensive error handling
  inputResult <- liftIO safeReadInput
  case inputResult of
    Left "EOF" -> do
      liftIO $ logInfo "Client disconnected (EOF)"
      liftIO exitSuccess
    Left errMsg -> do
      -- Handle input error and continue (don't exit for robustness)
      _ <- handleServerError Nothing (InputError (T.pack errMsg))
      connectionLoop  -- Continue processing
    
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

-- Safe parameter parsing with input validation
parseQueryArgs :: Value -> Parser (Text, Maybe Text)
parseQueryArgs = withObject "QueryArgs" $ \v -> do
  queryStr <- v .: "query"
  -- Validate query string length and content
  if T.length queryStr > 10000  -- Reasonable PromQL query limit
    then fail "Query string too long (>10KB)"
    else if T.null (T.strip queryStr)
      then fail "Query string cannot be empty"
      else do
        mTime <- v .:? "time"
        -- Validate time string if present
        case mTime of
          Just timeStr -> 
            if T.length timeStr > 100  -- Reasonable timestamp limit
              then fail "Time string too long"
              else return (queryStr, mTime)
          Nothing -> return (queryStr, mTime)

parseRangeQueryArgs :: Value -> Parser (Text, Text, Text, Text)
parseRangeQueryArgs = withObject "RangeQueryArgs" $ \v -> do
  queryStr <- v .: "query"
  start <- v .: "start"
  end <- v .: "end"
  step <- v .: "step"
  
  -- Validate all inputs
  if T.length queryStr > 10000
    then fail "Query string too long (>10KB)"
    else if T.null (T.strip queryStr)
      then fail "Query string cannot be empty"
      else if T.length start > 100 || T.length end > 100
        then fail "Timestamp strings too long"
        else if T.length step > 20
          then fail "Step string too long"
          else if T.null (T.strip start) || T.null (T.strip end) || T.null (T.strip step)
            then fail "Timestamp and step parameters cannot be empty"
            else return (queryStr, start, end, step)

parseSeriesArgs :: Value -> Parser ([Text], Text, Text)
parseSeriesArgs = withObject "SeriesArgs" $ \v -> do
  matchers <- v .: "match"
  start <- v .: "start"
  end <- v .: "end"
  
  -- Validate inputs
  if length matchers > 100  -- Reasonable limit on matchers
    then fail "Too many matchers (>100)"
    else if any (\m -> T.length m > 1000) matchers  -- Reasonable matcher length
      then fail "Matcher string too long (>1KB)"
      else if T.length start > 100 || T.length end > 100
        then fail "Timestamp strings too long"
        else if T.null (T.strip start) || T.null (T.strip end)
          then fail "Timestamp parameters cannot be empty"
          else return (matchers, start, end)

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
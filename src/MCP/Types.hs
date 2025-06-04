{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MCP.Types
  ( Request(..)
  , Response(..)
  , Method(..)
  , Tool(..)
  , ToolCall(..)
  , ToolResult(..)
  , Error(..)
  , encodeResponse
  , decodeRequest
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import GHC.Generics

data Request = Request
  { requestJsonrpc :: Text
  , requestId :: Maybe Value
  , requestMethod :: Method
  , requestParams :: Maybe Value
  } deriving (Show, Generic)

data Response = Response
  { responseJsonrpc :: Text
  , responseId :: Maybe Value
  , responseResult :: Maybe Value
  , responseError :: Maybe Error
  } deriving (Show, Generic)

data Method
  = Initialize
  | ListTools
  | CallTool
  | Notification
  | Unknown Text
  deriving (Show, Eq)

data Tool = Tool
  { toolName :: Text
  , toolDescription :: Text
  , toolInputSchema :: Value
  } deriving (Show, Generic)

data ToolCall = ToolCall
  { toolCallName :: Text
  , toolCallArguments :: Value
  } deriving (Show, Generic)

data ToolResult = ToolResult
  { toolResultContent :: [Value]
  , toolResultIsError :: Bool
  } deriving (Show, Generic)

data Error = Error
  { errorCode :: Int
  , errorMessage :: Text
  , errorData :: Maybe Value
  } deriving (Show, Generic)

instance FromJSON Request where
  parseJSON = withObject "Request" $ \v -> Request
    <$> v .: "jsonrpc"
    <*> v .:? "id"
    <*> (parseMethod <$> v .: "method")
    <*> v .:? "params"

parseMethod :: Text -> Method
parseMethod "initialize" = Initialize
parseMethod "tools/list" = ListTools
parseMethod "tools/call" = CallTool
parseMethod "notifications/initialized" = Notification
parseMethod other = Unknown other

instance ToJSON Response where
  toJSON Response{..} = object $ filter ((/= Null) . snd)
    [ "jsonrpc" .= responseJsonrpc
    , "id" .= responseId
    , "result" .= responseResult
    , "error" .= responseError
    ]

instance ToJSON Error where
  toJSON Error{..} = object $ filter ((/= Null) . snd)
    [ "code" .= errorCode
    , "message" .= errorMessage
    , "data" .= errorData
    ]

instance ToJSON Tool where
  toJSON Tool{..} = object
    [ "name" .= toolName
    , "description" .= toolDescription
    , "inputSchema" .= toolInputSchema
    ]

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \v -> ToolCall
    <$> v .: "name"
    <*> v .: "arguments"

instance ToJSON ToolResult where
  toJSON ToolResult{..} = object
    [ "content" .= toolResultContent
    , "isError" .= toolResultIsError
    ]

encodeResponse :: Response -> Value
encodeResponse = toJSON

decodeRequest :: Value -> Maybe Request
decodeRequest = parseMaybe parseJSON
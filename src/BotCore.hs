{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module BotCore (bot, Update) where

import GHC.Generics
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Control.Monad ((<=<))
import Network.Wreq (post)
import Data.Aeson
  ( ToJSON
  , FromJSON
  , toJSON
  , parseJSON
  , fieldLabelModifier
  , genericParseJSON
  , defaultOptions
  , genericToJSON
  )


data Chat = Chat
  { chat_id :: Int
  , chat_type :: String
  , chat_username :: Maybe String
  } deriving (Generic, Show)

instance ToJSON Chat where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = dropPrefix "chat_" }

instance FromJSON Chat where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = dropPrefix "chat_" }


data User = User
  { user_id :: Int
  , user_first_name :: String
  } deriving (Generic, Show)

instance ToJSON User where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = dropPrefix "user_" }

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = dropPrefix "user_" }


data Message = Message
  { message_message_id :: Int
  , message_from :: Maybe User
  , message_date :: Int
  , message_chat :: Chat
  , message_text :: Maybe String
  , message_pinned_message :: Maybe Message
  } deriving (Generic, Show)

instance ToJSON Message where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = dropPrefix "message_" }

instance FromJSON Message where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = dropPrefix "message_" }


data Update = Update
  { update_update_id :: Int
  , update_message :: Maybe Message
  } deriving (Generic, Show)

instance ToJSON Update where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = dropPrefix "update_" }

instance FromJSON Update where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = dropPrefix "update_" }


data Telegram = Telegram
  { ok :: Bool
  , result :: [Update]
  } deriving (Generic, ToJSON, FromJSON)


data SendMessage = SendMessage
  { sendmessage_chat_id :: String
  , sendmessage_text :: String
  } deriving (Generic)

instance ToJSON SendMessage where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = dropPrefix "sendmessage_" }

instance FromJSON SendMessage where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = dropPrefix "sendmessage_" }

dropPrefix :: String -> String -> String
dropPrefix prefix str = drop (length prefix) str


getToken :: IO String
getToken = do
  env <- lookupEnv "TOKEN"
  return $ fromMaybe "" env

api :: String -> String -> String
api token method = "https://api.telegram.org/bot" ++ token ++ method


getChatChannel :: Int -> Maybe (String, String)
getChatChannel id_ = lookup id_ table
  where table =
          [ (-1001169386594, ("testpinnergroup", "@testpinnerchannel"))
          , (0, ("test", "@test"))
          ]

redirectPinnedMessage :: Message -> Maybe SendMessage
redirectPinnedMessage message =
  let
    chatChannel = (getChatChannel . chat_id . message_chat) message
    message_id = message_message_id message
  in
    case chatChannel of
      Nothing -> Nothing
      Just (chat, channel) ->
        Just $ SendMessage channel
          ("https://t.me/" ++ chat ++ "/" ++ show message_id)


sendMessage :: SendMessage -> IO ()
sendMessage message = do
  token <- getToken
  _ <- post (api token "/sendMessage") (toJSON message)
  return ()


-- TODO: Make work only in rustjerk
bot :: Update -> IO ()
bot update =
  case update_message update of
    Nothing -> return ()
    Just message ->
      case message_pinned_message message of
        Nothing -> return ()
        Just pinned_message ->
          case redirectPinnedMessage pinned_message of
            Nothing -> return ()
            Just msg -> sendMessage msg

{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module BotCore (bot, Update) where

import GHC.Generics
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
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


data Message = Message
  { message_message_id :: Int
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


getChannel :: Chat -> Maybe String
getChannel chat = lookup (chat_id chat) table
  where table =
          [ (-1001169386594, "@testpinnerchannel")
          , (-1001313149703, "@rustquotesbr")
          ]

redirectPinnedMessage :: Message -> Maybe SendMessage
redirectPinnedMessage message = do
  let messageId = message_message_id message
  let chat = message_chat message
  channel <- getChannel chat
  chatUsername <- chat_username chat
  return $ SendMessage channel
    ("https://t.me/" ++ chatUsername ++ "/" ++ show messageId)


sendMessage :: SendMessage -> IO ()
sendMessage message = do
  token <- getToken
  _ <- post (api token "/sendMessage") (toJSON message)
  return ()

core :: Update -> Maybe SendMessage
core update = pure update
  >>= update_message
  >>= message_pinned_message
  >>= redirectPinnedMessage

bot :: Update -> IO ()
bot update =
  case core update of
    Nothing -> return ()
    Just msg -> sendMessage msg

\chapter{Messages}

\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Endpoints.Messages (MESSAGES, messageIO) where

import DB --(Message (..), Account (..))

import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.Aeson (FromJSON,ToJSON,encode)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import Database.Persist (insert
                        ,selectList
                        ,selectFirst
                        ,SelectOpt (..)
                        ,entityVal
                        ,(>.)
                        ,(<=.))
import Database.Persist.Sqlite (ConnectionPool,runSqlPersistMPool)
import GHC.Generics (Generic)
import Network.WebSockets (PendingConnection
                          ,Connection
                          ,sendTextData
                          ,withPingThread
                          ,acceptRequest)
import Servant ((:<|>) (..),(:>),Server,ReqBody,JSON,Get,Post)
import Servant.API.WebSocket (WebSocketPending)

data MessageRequest = MessageRequest
  { dateUntil :: UTCTime
  , count :: Word
  } deriving (Generic, Show)

instance FromJSON MessageRequest
instance ToJSON MessageRequest

data NewMessage = NewMessage
  { content :: Text
  } deriving (Generic, Show)

instance FromJSON NewMessage
instance ToJSON NewMessage

type MESSAGES = "messages" :> WebSocketPending
           :<|> "messages" :> ReqBody '[JSON] MessageRequest
                           :> Get '[JSON] [Message]
           :<|> "messages" :> ReqBody '[JSON] NewMessage
                           :> Post '[JSON] Message

messageIO :: ConnectionPool -> Account -> Server MESSAGES
messageIO o a = messageStream :<|> getMessages :<|> postMessage
  where
    messageStream :: MonadIO m => PendingConnection -> m ()
    messageStream w =
      let go :: MonadIO m => Connection -> Message -> m ()
          go c m = liftIO $ do
            l <- runSqlPersistMPool
                   (selectFirst [] [Desc MessageSent]) o
            case l of
              Just l' -> do
                let e = entityVal l'
                if e == m
                then go c m
                else do
                  let d = messageSent m
                  let d' = messageSent e
                  n <- runSqlPersistMPool
                         (selectList [MessageSent >. d
                                     ,MessageSent <=. d']
                                     []) o
                  sendTextData c $ encode $ entityVal <$> n
                  go c e
              _ -> return ()
      in liftIO $ do
        c <- acceptRequest w
        l <- runSqlPersistMPool (selectFirst [] [Desc MessageSent]) o
        case l of
          Just l' -> liftIO $ do
            withPingThread c 10 (return ()) (return ())
            go c $ entityVal l'
          _ -> return ()
    getMessages :: MonadIO m => MessageRequest -> m [Message]
    getMessages r = liftIO $ do
      let d = dateUntil r
      let c = fromInteger . toInteger $ Endpoints.Messages.count r
      m <- runSqlPersistMPool (selectList [MessageSent <=. d] [LimitTo c]) o
      return $ (entityVal <$> m)
    postMessage :: MonadIO m => NewMessage -> m Message
    postMessage m = liftIO $ do
      t <- getCurrentTime
      let c = content m
      let a' = accountIdentifier a
      let m' = Message a' c t
      _ <- runSqlPersistMPool (insert m') o
      return m'
\end{code}

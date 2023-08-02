\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Endpoints.Messages (MESSAGES, messageIO) where

import DB (Message (..))

import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.Text (empty)
import Data.Time.Clock (getCurrentTime)
import Database.Persist (insert)
import Database.Persist.Sqlite (ConnectionPool,runSqlPersistMPool)
import Network.WebSockets (PendingConnection
                          ,sendTextData
                          ,sendClose
                          ,withPingThread
                          ,receiveDataMessage
                          ,fromDataMessage
                          ,acceptRequest)
import Servant ((:<|>) (..),(:>),Get,Server)
import Servant.API.WebSocket (WebSocketPending)
import Servant.HTML.Blaze (HTML)
import Text.Blaze (ToMarkup (..))
import Text.Blaze.Html5 as H (head,title,body,p,docTypeHtml)

data MessagePage = MessagePage
instance ToMarkup MessagePage where
  toMarkup _ = docTypeHtml $ do
    H.head $ do
      title "Ibew Tupa: The Reckoning"
    body $ do
      p "Get them messages"

type MESSAGES = "app" :> Get '[HTML] MessagePage
           :<|> "messages" :> WebSocketPending

messageIO :: ConnectionPool -> Server MESSAGES
messageIO o = (return MessagePage) :<|> echo
  where
    echo :: MonadIO m => PendingConnection -> m ()
    echo w = liftIO $ do
      c <- acceptRequest w
      withPingThread c 10 (return ()) (return ())
      forever $ do
        z <- receiveDataMessage c
        let z' = fromDataMessage z
        if z' == empty
        then sendClose c empty
        else getCurrentTime
          >>= \t -> runSqlPersistMPool (insert $ Message z' t) o
                    >> sendTextData c z'
\end{code}

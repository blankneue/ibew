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
\end{code}

These are the first two characters in the reserved block.
They correspond to two commands for the eventual interpreter.
\begin{code}
getCmd, putCmd :: Char
getCmd = '\57344'
putCmd = '\57345'

type MESSAGES = "messages" :> WebSocketPending

messageIO :: ConnectionPool -> Server MESSAGES
messageIO o = echo
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

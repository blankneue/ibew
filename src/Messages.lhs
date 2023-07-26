\begin{code}
{-# LANGUAGE DataKinds                  #-}

module Messages where

import DB (Message (..))

import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.Text
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
import Servant
import Servant.API.WebSocket

type MESSAGES = "messages" :> WebSocketPending

messageIO :: ConnectionPool -> Server MESSAGES
messageIO o = echo
  where
    echo :: MonadIO m => PendingConnection -> m ()
    echo p = liftIO $ do
      c <- acceptRequest p
      withPingThread c 10 (return ()) (return ())
      forever $ do
        z <- receiveDataMessage c
        let z' = fromDataMessage z
        if z' == empty
        then sendClose c empty
        else do
          t <- getCurrentTime
          runSqlPersistMPool (insert $ Message z' t) o
          sendTextData c z'
          return ()
\end{code}

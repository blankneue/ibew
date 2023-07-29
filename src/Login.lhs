\begin{code}
{-# LANGUAGE DataKinds                  #-}

module Login where

import DB

import Data.Text
import Data.Text.Encoding
import Data.Time.Clock (getCurrentTime)
import Database.Persist (getBy)
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

type LOGIN = BasicAuth "Login" Account

loginIO :: ConnectionPool -> BasicAuthCheck Account
loginIO o = BasicAuthCheck $ \b ->
  let u = decodeUtf8' (basicAuthUsername b)
      p = decodeUtf8' (basicAuthPassword b)
  in case (u, p) of
       (Right u', Right p') -> do
         a <- runSqlPersistMPool (getBy $ UniqueID u') o
         print a
         return BadPassword
       _ -> return NoSuchUser

\end{code}

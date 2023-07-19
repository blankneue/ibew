\begin{code}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    , initDB
    ) where

import Prelude (IO, ($), (==), FilePath)

import Control.Monad
import Control.Monad.IO.Class
import Data.Text
import Database.SQLite.Simple
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Network.WebSockets
import Servant
import Servant.API.WebSocket

type API = "messages" :> WebSocketPending

startApp :: FilePath -> FilePath -> FilePath -> IO ()
startApp dbfile cert key = runTLS
  (tlsSettings cert key)
  (setPort 8080 defaultSettings)
  (serve api $ server dbfile)

api :: Proxy API
api = Proxy

server :: FilePath -> Server API
server dbfile = echo
  where
    echo :: MonadIO m => PendingConnection -> m ()
    echo pc = do
      c <- liftIO $ acceptRequest pc
      liftIO $ withPingThread c 10 (return ()) (return ())
      forever $ liftIO $ do
        z <- receiveDataMessage c
        let z' = fromDataMessage z
        if (z' == empty)
        then sendClose c empty
        else do
          withConnection dbfile $ \c ->
            execute c "INSERT INTO messages VALUES (?)"
            (Only z')
          sendTextData c z'

initDB :: FilePath -> IO ()
initDB dbfile = withConnection dbfile $ \conn ->
  execute_ conn
    "CREATE TABLE IF NOT EXISTS messages (msg text not null)"
\end{code}

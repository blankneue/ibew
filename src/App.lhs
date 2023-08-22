\chapter{App: The Top Level}

\begin{code}
module App
    ( run
    ) where

import Configuration
import DB
import Endpoints.Login
import Endpoints.Messages
import Endpoints.Headmates
import Endpoints.Register

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sqlite (ConnectionPool
                               ,runSqlPool
                               ,runMigration
                               ,createSqlitePool)
import Network.Wai.Handler.Warp (defaultSettings,setPort)
import Network.Wai.Handler.WarpTLS (tlsSettings,runTLS)
import Servant ((:<|>) (..)
               ,(:>)
               ,Context ((:.), EmptyContext)
               ,Server
               ,Proxy (..)
               ,serveWithContext)

type PRIVATE = MESSAGES
          :<|> HEADMATES
type APP = REGISTER
      :<|> LOGIN :> PRIVATE

api :: Proxy APP
api = Proxy

private :: ConnectionPool -> ConnectionPool -> Account -> Server PRIVATE
private m a a' = (messageIO m a a') :<|> (headmatesIO a a')

server :: ConnectionPool -> ConnectionPool -> Server APP
server m a = (registerIO a)
        :<|> (private m a)

run :: Configuration -> IO ()
run c = let fromNatural = fromInteger . toInteger
            s = fromNatural $ messagesPoolSize c
            p = fromNatural $ port c
            t = tlsSettings (certificate c) (privateKey c)
            e = setPort p defaultSettings
            v m a = serveWithContext api (ctx a) (server m a)
            ctx a = loginIO a :. EmptyContext
        in do
             (m,a) <- runStderrLoggingT $ do
               m <- createSqlitePool (messagesDB c) s
               a <- createSqlitePool (accountDB c) s
               return (m,a)
             runSqlPool (runMigration migrateMessage) m
             runSqlPool (runMigration migrateAccount) a
             runTLS t e $ v m a
\end{code}

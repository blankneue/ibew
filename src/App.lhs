\begin{code}
module App
    ( run
    ) where

import Configuration
import DB
import Landing
import Login
import Messages

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sqlite
import Network.Wai.Handler.Warp (defaultSettings,setPort)
import Network.Wai.Handler.WarpTLS (tlsSettings,runTLS)
import Servant


type APP = LANDING
      :<|> LOGIN :>
         ( MESSAGES
         )

api :: Proxy APP
api = Proxy

server :: ConnectionPool -> Server APP
server o = landing :<|> return (messageIO o)

run :: Configuration -> IO ()
run c = let fromNatural = fromInteger . toInteger
            s = fromNatural $ messagesPoolSize c
            p = fromNatural $ port c
            t = tlsSettings (certificate c) (privateKey c)
            e = setPort p defaultSettings
            v m a = serveWithContext api (ctx a) (server m)
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

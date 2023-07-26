\begin{code}
module App
    ( run
    ) where

import Configuration
import DB
import Messages

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sqlite
import Network.Wai.Handler.Warp (defaultSettings,setPort)
import Network.Wai.Handler.WarpTLS (tlsSettings,runTLS)
import Servant

type APP = MESSAGES

api :: Proxy APP
api = Proxy

server :: ConnectionPool -> Server APP
server = messageIO

run :: Configuration -> IO ()
run c = let fromNatural = fromInteger . toInteger
            s = fromNatural $ messagesPoolSize c
            p = fromNatural $ port c
            t = tlsSettings (certificate c) (privateKey c)
            e = setPort p defaultSettings
        in runStderrLoggingT (createSqlitePool (messagesDB c) s)
           >>= \o -> runSqlPool (runMigration migrateMessage) o
           >> runTLS t e (serve api $ server o)

\end{code}

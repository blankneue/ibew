Note to self: the case statement nightmare in the loginIO function could be fixed with a couple of monad transformers; EitherT and MaybeT.
\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Endpoints.Login (LOGIN, loginIO) where

import DB
import Hashing

import Crypto.Error (CryptoFailable (..))
import Data.ByteString.Char8 (pack)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8')
import Database.Persist (getBy)
import Database.Persist.Sqlite (ConnectionPool,runSqlPersistMPool,entityVal)
import Servant (BasicAuth
               ,BasicAuthCheck (..)
               ,BasicAuthResult (..)
               ,basicAuthUsername
               ,basicAuthPassword)

type LOGIN = BasicAuth "Ibew" Account

loginIO :: ConnectionPool -> BasicAuthCheck Account
loginIO o = BasicAuthCheck $ \b ->
  let u = decodeUtf8' (basicAuthUsername b)
      p = decodeUtf8' (basicAuthPassword b)
      toBS = pack . unpack
      gp = toBS . accountHash . entityVal
      gs = toBS . accountSalt . entityVal
      hc p' (Just c) = pc (argonHash p' (gs c)) c
      hc _ _ = return NoSuchUser
      pc (CryptoPassed h) c = if (gp c) == h
                              then return (Authorized $ entityVal c)
                              else return BadPassword
      pc _ _ = return BadPassword
  in case (u, p) of
       (Right u', Right p') ->
         runSqlPersistMPool (getBy $ AccountDef u') o
         >>= hc (toBS p')
       _ -> return NoSuchUser
\end{code}

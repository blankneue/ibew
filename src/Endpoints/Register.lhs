\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Endpoints.Register (REGISTER, registerIO) where

import DB
import Hashing

import Control.Monad.IO.Class (MonadIO,liftIO)
import Crypto.Error (CryptoFailable (..))
import qualified Data.ByteString as B (unpack)
import qualified Data.ByteString.Char8 as C (pack)
import Data.Aeson (FromJSON,ToJSON)
import Data.ByteString.Short (fromShort)
import Data.Char (chr)
import Data.Text (Text,pack,unpack)
import Database.Persist.Sqlite (ConnectionPool,runSqlPersistMPool,insert)
import GHC.Generics (Generic)
import Servant ((:>),Server,Post,ReqBody,JSON)
import System.Random (initStdGen,genShortByteString)

data AccountRequirements = AccountRequirements
  { identifier :: Text
  , password :: Text
  } deriving (Generic, Show)

instance FromJSON AccountRequirements
instance ToJSON AccountRequirements

data RegistrationResult = RegistrationResult
  { success :: Bool
  , error :: Text
  } deriving (Generic, Show)

instance FromJSON RegistrationResult
instance ToJSON RegistrationResult

type REGISTER = "register" :> ReqBody '[JSON] AccountRequirements
                           :> Post '[JSON] RegistrationResult

registerIO :: ConnectionPool -> Server REGISTER
registerIO o = register
  where
    register :: MonadIO m => AccountRequirements -> m RegistrationResult
    register (AccountRequirements i w) =
      let toText x = pack $ (chr . fromIntegral) <$> (B.unpack x)
          hp (CryptoPassed hash') salt = do
            let h = toText hash'
            let s = toText salt
            _ <- liftIO $ runSqlPersistMPool (insert $ Account i h s) o
            return $ RegistrationResult True ""
          hp _ _ = return $ RegistrationResult False "Cryptography failed"
      in do
        r <- initStdGen
        let pass = (C.pack . unpack) w
        let salt = fromShort . fst $ genShortByteString 512 r
        let hash = argonHash pass salt
        hp hash salt
\end{code}

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
import Servant ((:<|>) (..),(:>),Server,Get,Post,ReqBody,JSON)
import Servant.HTML.Blaze (HTML)
import System.Random (initStdGen,genShortByteString)
import Text.Blaze (ToMarkup (..))
import Text.Blaze.Html5 as H (docTypeHtml,head,title,body,p)

data AccountRequirements = AccountRequirements
  { identifier :: Text
  , password :: Text
  } deriving (Generic, Show)

instance FromJSON AccountRequirements
instance ToJSON AccountRequirements

data RegisterPage = RegisterPage
instance ToMarkup RegisterPage where
  toMarkup _ = docTypeHtml $ do
    H.head $ do
      H.title "Ibew Tupa 2: Electric Boogaloo"
    body $ do
      p "Sign up now!"

data RegisterStatus = RegisterFailure | RegisterSuccess

data RegisteredPage = RegisteredPage RegisterStatus
instance ToMarkup RegisteredPage where
  toMarkup (RegisteredPage RegisterSuccess) = docTypeHtml $ do
      H.head $ do
        H.title "Ibew Tupa 3: The Return of K.K. Slider"
      body $ do
        p "Signed up!"
  toMarkup (RegisteredPage RegisterFailure) = docTypeHtml $ do
        H.head $ do
          H.title "Ibew Tupa 3: The Return of K.K. Slider"
        body $ do
          p "Sorry, signing up failed--please try again."

type REGISTER = "register" :> Get '[HTML] RegisterPage
           :<|> "register" :> ReqBody '[JSON] AccountRequirements
                           :> Post '[HTML] RegisteredPage

registerIO :: ConnectionPool -> Server REGISTER
registerIO o = (return RegisterPage) :<|> register
  where
    toText x = pack $ (chr . fromIntegral) <$> (B.unpack x)
    register :: MonadIO m => AccountRequirements -> m RegisteredPage
    register (AccountRequirements i w) =
      let hp (CryptoPassed hash') salt = do
            let h = toText hash'
            let s = toText salt
            _ <- liftIO $ runSqlPersistMPool (insert $ Account i h s) o
            return $ RegisteredPage RegisterSuccess
          hp _ _ = return $ RegisteredPage RegisterFailure
      in do
        r <- initStdGen
        let pass = (C.pack . unpack) w
        let salt = fromShort . fst $ genShortByteString 512 r
        let hash = argonHash pass salt
        hp hash salt
\end{code}

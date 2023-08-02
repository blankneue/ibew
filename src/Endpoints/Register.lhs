\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Endpoints.Register where

import DB
import Hashing

import Control.Monad.IO.Class (MonadIO,liftIO)
import Crypto.Error (CryptoFailable (..))
import qualified Data.ByteString as B (unpack)
import qualified Data.ByteString.Char8 as C (pack)
import Data.ByteString.Short (fromShort)
import Data.Char (chr)
import Data.String (fromString)
import Data.Text (Text,pack,unpack)
import Database.Persist.Sqlite (ConnectionPool,runSqlPersistMPool,insert)
import Servant ((:<|>) (..),(:>),Server,Get,Post)
import Servant.HTML.Blaze (HTML)
import Servant.Multipart (MultipartForm,MultipartData,Mem,iName,iValue,inputs)
import System.Random (initStdGen,genShortByteString)
import Text.Blaze (ToMarkup (..),(!))
import Text.Blaze.Html5 as H (docTypeHtml,head,title,body,p,form,input)
import Text.Blaze.Html5.Attributes as A (method,enctype,type_,name)

identifierKey, passwordKey :: String
identifierKey = "identifier"
passwordKey = "password"

data RegisterPage = RegisterPage
instance ToMarkup RegisterPage where
  toMarkup _ = docTypeHtml $ do
    H.head $ do
      H.title "Ibew Tupa 2: Electric Boogaloo"
    body $ do
      p "Sign up now!"
      H.form ! method "POST" ! enctype "multipart/form-data" $ do
        input ! type_ "text" ! name (fromString identifierKey)
        input ! type_ "password" ! name (fromString passwordKey)
        input ! type_ "submit"

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
           :<|> "register" :> MultipartForm Mem (MultipartData Mem)
                           :> Post '[HTML] RegisteredPage

registerIO :: ConnectionPool -> Server REGISTER
registerIO o = (return RegisterPage) :<|> register
  where
    register :: MonadIO m => MultipartData Mem -> m RegisteredPage
    register z =
      let gatherIdentifier y = gatherKey y (fromString identifierKey)
          gatherPassword y = gatherKey y (fromString passwordKey)
          gatherKey :: MultipartData Mem -> Text -> Maybe Text
          gatherKey x y = case filter (\k -> iName k == y) (inputs x) of
            k:[] -> Just (iValue k)
            _ -> Nothing
          i = gatherIdentifier z
          w = gatherPassword z
          toText x = pack $ (chr . fromIntegral) <$> (B.unpack x)
          hp (CryptoPassed hash') salt i' = do
            let h = toText hash'
            let s = toText salt
            _ <- liftIO $ runSqlPersistMPool (insert $ Account i' h s) o
            return $ RegisteredPage RegisterSuccess
          hp _ _ _ = return $ RegisteredPage RegisterFailure
      in case (i,w) of
        (Just i', Just w') -> do
          r <- initStdGen
          let pass = (C.pack . unpack) w'
          let salt = fromShort . fst $ genShortByteString 512 r
          let hash = argonHash pass salt
          hp hash salt i'
        _ -> return $ RegisteredPage RegisterFailure
\end{code}

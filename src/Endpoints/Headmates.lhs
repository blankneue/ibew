\chapter{Headmates}

\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Endpoints.Headmates (HEADMATES, headmatesIO) where

import DB

import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.Aeson (FromJSON,ToJSON)
import Data.Text (Text)
import Database.Persist ((==.),entityKey,Entity (..))
import Database.Persist.Sqlite (ConnectionPool
                               ,runSqlPersistMPool
                               ,insert
                               ,selectFirst
                               ,selectList)
import GHC.Generics (Generic)
import Servant ((:<|>) (..),(:>),Server,Post,Get,ReqBody,JSON)

data ClientHeadmate = ClientHeadmate
  { name :: Text
  , tagHead :: Text
  , tagTail :: Text
  } deriving (Generic, Show)

instance FromJSON ClientHeadmate
instance ToJSON ClientHeadmate

clientHeadmate :: Entity Headmate -> ClientHeadmate
clientHeadmate (Entity _ (Headmate _ n th tt)) = ClientHeadmate n th tt

type HEADMATES = "headmates" :> ReqBody '[JSON] ClientHeadmate
                             :> Post '[JSON] (Maybe ClientHeadmate)
            :<|> "headmates" :> Get '[JSON] [ClientHeadmate]

headmatesIO :: ConnectionPool -> Account -> Server HEADMATES
headmatesIO o a = headmateCreate :<|> headmatesGet
  where
    headmateCreate :: MonadIO m => ClientHeadmate -> m (Maybe ClientHeadmate)
    headmateCreate h = liftIO $ do
      let i = accountIdentifier a
      a' <- runSqlPersistMPool
        (selectFirst [AccountIdentifier ==. i] []) o
      case a' of
        Just i' -> do
          let h' = Headmate (entityKey i') (name h) (tagHead h) (tagTail h)
          _ <- runSqlPersistMPool (insert h') o
          return $ Just h
        _ -> return Nothing
    headmatesGet :: MonadIO m => m [ClientHeadmate]
    headmatesGet = liftIO $ do
      let i = accountIdentifier a
      a' <- runSqlPersistMPool
        (selectFirst [AccountIdentifier ==. i] []) o
      case a' of
        Just i' -> do
          h <- runSqlPersistMPool
            (selectList [HeadmateAccountId ==. (entityKey i')] []) o
          return $ clientHeadmate <$> h
        _ -> return []
\end{code}

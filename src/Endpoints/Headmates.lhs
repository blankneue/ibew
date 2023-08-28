\chapter{Headmates}

\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Endpoints.Headmates (HEADMATES, headmatesIO) where

import DB

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.Aeson (FromJSON,ToJSON)
import Data.Text (Text)
import Database.Persist ((==.),entityKey,Entity (..))
import Database.Persist.Sqlite (ConnectionPool
                               ,runSqlPersistMPool
                               ,insert
                               ,selectList)
import GHC.Generics (Generic)
import Servant ((:<|>) (..)
               ,(:>)
               ,Server
               ,Post
               ,Get
               ,ReqBody
               ,JSON
               ,ServerError
               ,throwError
               ,err403)

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
                             :> Post '[JSON] ClientHeadmate
            :<|> "headmates" :> Get '[JSON] [ClientHeadmate]

headmatesIO :: ConnectionPool -> Account -> Server HEADMATES
headmatesIO o a = headmateCreate :<|> headmatesGet
  where
    headmateCreate :: (MonadIO m, MonadError ServerError m)
                   => ClientHeadmate
                   -> m ClientHeadmate
    headmateCreate h = do
      a' <- liftIO $ accountKey a o
      case a' of
        Just i' -> do
          let h' = Headmate (entityKey i') (name h) (tagHead h) (tagTail h)
          _ <- liftIO $ runSqlPersistMPool (insert h') o
          return h
        _ -> throwError err403
    headmatesGet :: (MonadIO m, MonadError ServerError m) => m [ClientHeadmate]
    headmatesGet = do
      a' <- liftIO $ accountKey a o
      case a' of
        Just i' -> do
          h <- liftIO $ runSqlPersistMPool
            (selectList [HeadmateAccountId ==. (entityKey i')] []) o
          return $ clientHeadmate <$> h
        _ -> throwError err403
\end{code}

\chapter{Messages}

\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Endpoints.Messages (MESSAGES, messageIO) where

import DB
import TagParse

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.Aeson (FromJSON,ToJSON,encode)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import Database.Persist (insert
                        ,selectList
                        ,selectFirst
                        ,SelectOpt (..)
                        ,entityVal
                        ,Entity (..)
                        ,(>.)
                        ,(<=.)
                        ,(==.))
import Database.Persist.Sqlite (ConnectionPool,runSqlPersistMPool)
import GHC.Generics (Generic)
import Servant ((:<|>) (..)
               ,(:>)
               ,Server
               ,ReqBody
               ,JSON
               ,Get
               ,Post
               ,ServerError
               ,throwError
               ,err422
               ,err403
               ,StreamGet
               ,NewlineFraming
               ,SourceIO)
import Servant.Types.SourceT

data MessageRequest = MessageRequest
  { dateUntil :: UTCTime
  , count :: Word
  } deriving (Generic, Show)

instance FromJSON MessageRequest
instance ToJSON MessageRequest

data NewMessage = NewMessage
  { content :: Text
  , headmateName :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON NewMessage
instance ToJSON NewMessage

type MESSAGES = "messages" :> StreamGet NewlineFraming JSON (SourceIO Message)
           :<|> "messages" :> ReqBody '[JSON] MessageRequest
                           :> Get '[JSON] [Message]
           :<|> "messages" :> ReqBody '[JSON] NewMessage
                           :> Post '[JSON] Message

messageIO :: ConnectionPool -> ConnectionPool -> Account -> Server MESSAGES
messageIO o p a = messageStream :<|> getMessages :<|> postMessage
\end{code}

The most complicated one is the HTTP Streaming endpoint.
This is what will keep a user up-to-date on the latest messages.
The method I'm doing that with is:
1. Get the latest message
2. Send the messages newer than that one to the user
3. Recurse with the last one from that list as the newest "latest message"
The issue I'm encountering is, how do I fit that into the SourceIO datatype?
I can't call a recursion and append it to the list, because it's effectful.
So, I have to find some other way.
It has to repeat forever, until the user disconnects, so I can't call a recursion before the final return--the recursion has to be part of the return.
So maybe I need a different design?
How does the stream `end' anyway?

A rethink is what's needed.
We'll need to study SourceT a bit more so we can grok what it's expecting.
Once we have that, we might have a smarter design in mind.

\begin{code}
  where
    messageStream =
      let go m = go m
      in liftIO $ do
          l <- runSqlPersistMPool (selectFirst [] [Desc MessageSent]) o
          case l of
            Just l' -> go (entityVal l')
            _ -> return $ source []
\end{code}

\begin{code}
    getMessages :: MonadIO m => MessageRequest -> m [Message]
    getMessages r = liftIO $ do
      let d = dateUntil r
      let c = fromInteger . toInteger $ Endpoints.Messages.count r
      m <- runSqlPersistMPool (selectList [MessageSent <=. d] [LimitTo c]) o
      return $ (entityVal <$> m)
\end{code}

Post message has three possibilities: they provide a headmate (selected by the client), or no headmate is provided, so it either indicates a headmate with tags, or is expected to use the default appearance.

\begin{code}
    postMessage :: (MonadIO m, MonadError ServerError m)
                => NewMessage
                -> m Message
\end{code}

We'll start with the supplied headmate.
The steps to validate this are simple: gather the account, validate the headmate is one of theirs, and add the message.
If a failure is encountered along that route, send an error code back.

\begin{code}
    postMessage (NewMessage m (Just h)) = do
      a' <- liftIO $ accountKey a p
      case a' of
        Just i -> do
          let i' = entityKey i
          h' <- liftIO $ runSqlPersistMPool
            (selectFirst [HeadmateName ==. h
                         ,HeadmateAccountId ==. i'] []) p
          case h' of
            Just h'' -> do
              t <- liftIO $ getCurrentTime
              let m' = Message i' (Just $ entityKey h'') m t
              _ <- liftIO $ runSqlPersistMPool (insert m') o
              return  m'
            _ -> throwError err422
        _ -> throwError err403
\end{code}

In this block, the headmate was not provided.
That means they either provided tags, or they want to send a message with the default appearance.
So, we gather the account, list the headmates, parse the message for each possible tag, and if none match, we simply pass back a Nothing.

\begin{code}
    postMessage (NewMessage m Nothing) = do
      a' <- liftIO $ accountKey a p
      case a' of
        Just i -> do
          let i' = entityKey i
          h <- liftIO $ runSqlPersistMPool
            (selectList [HeadmateAccountId ==. i'] []) p
          t <- liftIO $ getCurrentTime
          let t' = (checkMessage . headmateTag <$> h) <*> (headmateName' <$> h)
          if or t'
          then return $ Message i' Nothing m t
          else do
            let h' = snd $ head $ filter (\(x,_) -> x) $ zip t' h
            return $ Message i' (Just $ entityKey h') m t
        _ -> throwError err403

headmateTag :: Entity Headmate -> (Text, Text)
headmateTag (Entity _ (Headmate _ _ th tt)) = (th,tt)
headmateName' :: Entity Headmate -> Text
headmateName' (Entity _ (Headmate _ n _ _)) = n
\end{code}

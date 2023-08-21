\chapter{Configuring Ibew}

\begin{code}
{-# LANGUAGE DeriveGeneric      #-}

module Configuration (Configuration (..)) where

import Dhall (FromDhall, Text, Natural, Generic)

data Configuration = Configuration
  { port :: Natural
  , accountDB :: Text
  , accountPoolSize :: Natural
  , messagesDB :: Text
  , messagesPoolSize :: Natural
  , certificate :: FilePath
  , privateKey :: FilePath
  } deriving (Generic, Show)

instance FromDhall Configuration

\end{code}

\begin{code}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Configuration (Configuration (..)) where

import Dhall

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

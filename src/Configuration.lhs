\begin{code}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Configuration where

import Dhall

data Configuration = Configuration
  { port :: Natural
  , messagesDB :: Text
  , messagesPoolSize :: Natural
  , certificate :: FilePath
  , privateKey :: FilePath
  } deriving (Generic, Show)

instance FromDhall Configuration

\end{code}

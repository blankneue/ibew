\chapter{Database Definitions}

\begin{code}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE UndecidableInstances       #-}

module DB where

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH (share
                           ,mkPersist
                           ,sqlSettings
                           ,mkMigrate
                           ,persistLowerCase)

share [mkPersist sqlSettings, mkMigrate "migrateAccount"] [persistLowerCase|
Account
  identifier Text
  hash Text
  salt Text
  AccountDef identifier
  deriving Eq
  deriving Show

Headmate
  accountId AccountId
  name Text
  tagHead Text
  tagTail Text
  HeadmateDef accountId name tagHead tagTail
  deriving Eq
  deriving Show
|]

$(deriveJSON defaultOptions ''Account)

$(deriveJSON defaultOptions ''Headmate)

share [mkPersist sqlSettings, mkMigrate "migrateMessage"] [persistLowerCase|
Message
  identifier Text
  content Text
  sent UTCTime
  deriving Eq
  deriving Show
|]

$(deriveJSON defaultOptions ''Message)
\end{code}

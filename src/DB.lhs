\begin{code}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE UndecidableInstances       #-}
\end{code}


module DB (Account (..), migrateAccount
          ,Message (..), migrateMessage) where

\begin{code}
module DB where

import Data.Aeson.TH
import Data.Text
import Data.Time
import Database.Persist.TH
import Database.Persist.Sqlite

share [mkPersist sqlSettings, mkMigrate "migrateAccount"] [persistLowerCase|
Account
  identifier Text
  hash Text
  salt Text
  UniqueID identifier
  deriving Eq
  deriving Show
|]

$(deriveJSON defaultOptions ''Account)

share [mkPersist sqlSettings, mkMigrate "migrateMessage"] [persistLowerCase|
Message
  content Text
  sent UTCTime
  deriving Eq
  deriving Show
|]

$(deriveJSON defaultOptions ''Message)

\end{code}

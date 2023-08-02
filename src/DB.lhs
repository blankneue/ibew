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

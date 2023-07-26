\begin{code}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE UndecidableInstances       #-}

module DB (Message (..), migrateMessage) where

import Data.Aeson.TH
import Data.Text
import Data.Time
import Database.Persist.TH
import Database.Persist.Sqlite

share [mkPersist sqlSettings, mkMigrate "migrateMessage"] [persistLowerCase|
Message
  content Text
  sent UTCTime
  deriving Eq
  deriving Show
|]

$(deriveJSON defaultOptions ''Message)

\end{code}

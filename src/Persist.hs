module Persist
  (
  ) where

import Database.Persist

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Data
    text T.Text
    embedding B.ByteString
    class Int
    visited Int
    isDev Bool
    deriving Show
|]

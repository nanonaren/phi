{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Train
  (
    Train (..)
  , TrainParams (..)
  , TokenizeOption (..)
  , initialize
  ) where

import           Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as LB
import           Data.Csv.Streaming
import           Data.Foldable           (for_)
import qualified Data.Text               as T
import           Database.Persist
import           Database.Persist.Sqlite (runMigration, runSqlite)
import           Database.Persist.TH
import           System.Directory        (createDirectory)
import           System.IO.Error         (catchIOError, isAlreadyExistsError)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TrainData
    text T.Text
    embedding B.ByteString
    class Int
    visited Int
    isDev Bool
    deriving Show
|]


data Train = Train
             {
               trainData      :: FilePath
             , params         :: TrainParams
             , tokenizeOption :: TokenizeOption
             }
           | Retrain
             {
               directory :: FilePath
             }

data TokenizeOption = Character

data TrainParams = TrainParams
  {
    saveDirectory :: FilePath
  , numColours    :: Int
  }

initialize :: Train -> IO ()
initialize config@Train{} = do
  let dir = saveDirectory (params config)
  catchIOError (createDirectory dir) $ \err ->
    if isAlreadyExistsError err
    then error ("The directory `" ++ dir ++ "` already exists!")
    else error (show err)

  runSqlite (T.pack $ dir ++ "/data.db") $ do
    runMigration migrateAll
    dat <- liftIO $ LB.readFile (trainData config)
    for_ (decode HasHeader dat) $ \(string :: T.Text, label :: Int, isDev :: Int) -> do
      -- case tokenizeOption config of
      --   Character -> show string
      liftIO $ putStrLn (show string ++ "(" ++ show label ++ ")" ++ ", " ++ show isDev)
initialize _ = error "not possible"

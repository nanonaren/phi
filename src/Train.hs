{-# LANGUAGE ScopedTypeVariables #-}

module Train
  (
    Train (..)
  , TrainParams (..)
  , TokenizeOption (..)
  , initialize
  ) where

import qualified Data.ByteString.Lazy as B
import           Data.Csv.Streaming
import           Data.Foldable        (forM_)
import qualified Data.Text            as T

data Train = Train
             {
               trainData      :: FilePath
             , devData        :: FilePath
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
  dat <- B.readFile (trainData config)
  forM_ (decode HasHeader dat) $ \(string :: T.Text, label :: Int) -> do
    putStrLn (show string ++ "(" ++ show label ++ ")")
initialize _ = error "not possible"

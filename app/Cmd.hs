module Cmd
  (
    cmd
  , TrainParams (..)
  , Cmd (..)
  ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data TrainParams = TrainParams
  {
    saveDirectory :: FilePath
  , numColours    :: Int
  }

data Cmd = Train
           {
             trainData :: FilePath
           , devData   :: FilePath
           , params    :: TrainParams
           }
         | Retrain
           {
             directory :: FilePath
           }

trainParams :: Parser TrainParams
trainParams = TrainParams
  <$> strOption (long "directory" <> metavar "DIR" <> help "Directory for saving model")
  <*> option auto (long "numColours" <> metavar "INT" <> help "Number of colours in the model")

train :: Parser Cmd
train = Train
  <$> strOption (long "trainData" <> metavar "FILE" <> help "Train data file")
  <*> strOption (long "devData" <> metavar "FILE" <> help "Dev data file")
  <*> trainParams

retrain :: Parser Cmd
retrain = Retrain
  <$> strOption (long "load" <> metavar "DIR" <> help "The directory to load from")

cmd :: ParserInfo Cmd
cmd = info ((train <|> retrain) <**> helper)
  ( fullDesc
    <> progDesc "Finds a suitable sequence embedding"
    <> header "phi - Sequence embedding")

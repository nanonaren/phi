module Cmd
  (
    cmd
  , TrainParams (..)
  ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative
import           Train

tokenize :: Parser TokenizeOption
tokenize =
  flag Character Character (long "character" <> help "Make each character a token")

trainParams :: Parser TrainParams
trainParams = TrainParams
  <$> strOption (long "directory" <> metavar "DIR" <> help "Directory for saving model")
  <*> option auto (long "numColours" <> metavar "INT" <> help "Number of colours in the model")

train :: Parser Train
train = Train
  <$> strOption (long "trainData" <> metavar "FILE" <> help "Train data file")
  <*> strOption (long "devData" <> metavar "FILE" <> help "Dev data file")
  <*> trainParams
  <*> tokenize

retrain :: Parser Train
retrain = Retrain
  <$> strOption (long "load" <> metavar "DIR" <> help "The directory to load from")

cmd :: ParserInfo Train
cmd = info ((train <|> retrain) <**> helper)
  ( fullDesc
    <> progDesc "Finds a suitable sequence embedding"
    <> header "phi - Sequence embedding")

module Main where

import           Cmd
import           Options.Applicative (execParser)
import           Tokenize
import qualified TrainModel          as T

main :: IO ()
main = do
  -- options <- execParser cmd
  T.initialize T.ModelConfig{
    numColours = 10,
    maxTokens = 100,
    tokenizeConfig = Character,
    directory = "run1",
    trainData = "data.csv"}
  return ()
  -- case options of
  --   t@Train{} -> T.initialize {}
  --   Retrain{} -> putStrLn "Re-training"

module Main where

import           Cmd
import           Options.Applicative (execParser)

main :: IO ()
main = do
  options <- execParser cmd
  case options of
    Train{}   -> putStrLn "Training"
    Retrain{} -> putStrLn "Re-training"

module Main where

import           Cmd
import           Options.Applicative (execParser)
import           Train

main :: IO ()
main = do
  options <- execParser cmd
  case options of
    t@Train{} -> initialize t
    Retrain{} -> putStrLn "Re-training"

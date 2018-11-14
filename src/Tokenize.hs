module Tokenize
  (
    tokenize
  , TokenizeConfig(..)
  ) where


import           Data.Text            (Text, singleton)
import           Text.Megaparsec
import           Text.Megaparsec.Char (printChar)


data TokenizeConfig = Character


tokenize :: TokenizeConfig -> Text -> Maybe [Text]
tokenize config = parseMaybe (tokenize' config)

tokenize' :: TokenizeConfig -> Parsec () Text [Text]
tokenize' Character = (singleton <$>) <$> some printChar

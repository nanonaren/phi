module Tokenize
  (
    tokenize
  , TokenizeConfig(..)
  ) where


import           Data.Text (Text, singleton)


data TokenizeConfig = Character


tokenizer :: TokenizeConfig -> Text -> Maybe [Text]
tokenizer Character = parseMaybe (many printChar)

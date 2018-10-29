import           Data.Semigroup      ((<>))
import           Options.Applicative

data Train = Train
  {
    saveDirectory :: Filepath
  , trainData     :: Filepath
  , devData       :: Filepath
  , trainDataHash :: Maybe String
  , devDataHash   :: Maybe String
  , numColours    :: Int
  }

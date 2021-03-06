module Model
  (
    Model (..)
  ) where

import           Control.Monad       (forM)
import           Data.Array.IO       (IOArray, newArray)
import           Data.Array.Unboxed  (UArray)
import qualified Data.HashMap.Strict as H
import           Data.Text           as T

data Lambda = LambdaF (UArray (Int, Int, Int) Int)
  | LambdaM (IOArray (Int, Int, Int) Int)

data Frozen
data Thawed

data Model a = Model
  {
    numColours :: Int
  , token2Id   :: H.HashMap T.Text Int
  , lambda     :: Lambda
  }


new :: Int -> Int -> IO (Model Thawed)
new maxEmbeddings numC = (Model numC H.empty . LambdaM)
  <$> newArray ((0, 0, 0), (maxEmbeddings, numC, 2)) 0


addSample :: Model Thawed -> T.Text -> IO (Model Thawed)
addSample model txt = do
  -- randomize embedding
  _ <- error "tokenize and randomly initialize" -- forM tokenize (tokenizer model) txt $ \token -> do
  --  undefined
  return model



{-
lambda1+,..., lambdaN+, lambda1-,..., lambdaN- <- Gamma(alpha, beta)
for 1 <= i <= N
  for 1 <= c <= numColours
    for 1 <= s <= 2
      value <- Poisson(lambda_c_s)
-}

-- op x y = listArray (bounds x) $ do
--   c <- [0..nc-1]
--   [((c, 0), y ! (c, 0) + max 0 (x ! (c, 0) - y ! (c, 1))),
--    ((c, 1), x ! (c, 1) + max 0 (y ! (c, 1) - x ! (c, 0)))]
--   where (nc, 2) = bounds x


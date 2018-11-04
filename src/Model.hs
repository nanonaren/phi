module Model
  (
    Model (..)
  ) where

import           Data.HashMap.Strict as H
import           Data.Text           as T

data Lambda = LambdaF (Array (Int, Int, Int) Int)
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
new maxEmbeddings numC = (Model num H.empty . LambdaM)
  <$> newArray ((0, 0, 0), (maxEmbeddings, numC, 2)) 0


addSample :: Model Thawed -> T.Text -> IO (UArray (Int, Int) Int)
addSample model txt = do
  undefined


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


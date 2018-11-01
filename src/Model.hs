module Model
  (
    Model (..)
  ) where

import           Data.HashMap.Strict as H
import           Data.Text           as T

data Model = Model
  {
    numColours :: Int
  , token2Id   :: H.HashMap T.Text Int
  }

{-
lambda1+,..., lambdaN+, lambda1-,..., lambdaN- <- Gamma(alpha, beta)
for 1 <= i <= N
  for 1 <= c <= numColours
    for 1 <= s <= 2
      value <- Poisson(lambda_c_s)
-}

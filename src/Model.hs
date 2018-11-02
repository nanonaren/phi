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

-- op x y = computeS $ fromFunction sh f
--   where f (Z :. c :. 0) = y ! (Z :. c :. 0) + max 0 (x ! (Z :. c :. 0) - y ! (Z :. c :. 1))
--         f (Z :. c :. 1) = x ! (Z :. c :. 1) + max 0 (y ! (Z :. c :. 1) - x ! (Z :. c :. 0))


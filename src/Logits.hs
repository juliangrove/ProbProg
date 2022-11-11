{-# LANGUAGE RebindableSyntax #-}

module Logits where

-- The problem we solve here is to represent numbers very close to 0 with sufficient precision.

import Algebra.Classes
import Prelude as Prel hiding (Num(..), (/), fromRational)

newtype Logit = ExpNeg {negLog :: Double} deriving (Eq)

instance Ord Logit where
  compare (ExpNeg a) (ExpNeg b) = compare b a

fromLogit :: Logit -> Double
fromLogit (ExpNeg x) = Prel.exp (negate x)

toLogit :: Double -> Logit
toLogit x = ExpNeg (negate (Prel.log x))

instance Additive Logit where
  zero = ExpNeg (1/0)
  ExpNeg a + ExpNeg b | a <= b    = ExpNeg (a - Prel.log (1 + Prel.exp (a-b)))
                      | otherwise = ExpNeg (b - Prel.log (1 + Prel.exp (b-a)))

instance Multiplicative Logit where
  one = ExpNeg zero
  ExpNeg x * ExpNeg y = ExpNeg (x + y)


instance AbelianAdditive Logit

instance Division Logit where
  recip (ExpNeg x) = ExpNeg (negate x)

instance Show Logit where
  show  = show . fromLogit

-- >>> one::Logit
-- 1.0

-- >>> zero::Logit
-- 0.0

-- >>> (zero + toLogit 0.75)
-- 0.75

-- >>> zero == (zero::Logit)
-- True

-- >>> (toLogit 0.75 + ExpNeg 1000000000)
-- 0.75


-- >>> negLog one
-- 0.0

-- >>> negLog zero
-- Infinity


-- >>> negLog one
-- 0.0

-- >>> one * zero :: Logit
-- 0.0

-- >>> zero * zero :: Logit
-- 0.0

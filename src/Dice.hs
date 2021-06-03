{-# LANGUAGE FlexibleInstances #-}

module Dice where

import System.Random
import Control.Monad
import Types

d3,d4,d6,d8,d10,d12,d20 :: Dice
d3  = D 3
d4  = D 4
d6  = D 6
d8  = D 8
d10 = D 10
d12 = D 12
d20 = D 20

rollIO :: Dice -> IO Int
rollIO (C n) = return n
rollIO (D n) = randomRIO (1,n)
rollIO (NOf n dice) = sum <$> replicateM n (rollIO dice)
rollIO (Add l r) = liftM2 (+) (rollIO l) (rollIO r)
rollIO (Mul l r) = liftM2 (*) (rollIO l) (rollIO r)
rollIO (Sub l r) = liftM2 (-) (rollIO l) (rollIO r)

instance Num Dice where
    (+) = Add
    (*) = Mul
    (-) = Sub
    abs = undefined
    signum = undefined
    fromInteger = C . fromInteger

  {-
  The num instance for (Dice -> Dice)
  is a hack to allow writing expresions
  like 2 d6
  -}

instance Num (Dice -> Dice) where
  fromInteger n = NOf (fromInteger n)
  (+)    = error "dice litteral hack went awry"
  (*)    = error "dice litteral hack went awry"
  (-)    = error "dice litteral hack went awry"
  abs    = error "dice litteral hack went awry"
  signum = error "dice litteral hack went awry"


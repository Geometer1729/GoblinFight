module Dice where

import System.Random
import Control.Monad

data Dice = D Int | C Int | Add Dice Dice | Mul Dice Dice | Sub Dice Dice deriving Show

d :: Int -> Dice
d = D

instance Num Dice where
    (+) = Add
    (*) = Mul
    (-) = Sub
    abs = undefined
    signum = undefined
    fromInteger = C . fromInteger

rollIO :: Dice -> IO Int
rollIO (C n) = return n
rollIO (D n) = randomRIO (1,n)
rollIO (Add l r) = liftM2 (+) (rollIO l) (rollIO r)
rollIO (Mul l r) = liftM2 (*) (rollIO l) (rollIO r)
rollIO (Sub l r) = liftM2 (-) (rollIO l) (rollIO r)

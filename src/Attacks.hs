{-# LANGUAGE
  TemplateHaskell
#-}

module Attacks where

import Dice
import Control.Lens

type Damage = (DamageType,Dice)

data Attack = Attack{
  _bonus   :: (Int,Int,Int),
  _dmg     :: Damage,
  _critDmg :: Damage
                    }deriving Show


data DamageType =
    B
  | P
  | S
  | Fire
  | Acid
  deriving Show

makeLenses ''Attack

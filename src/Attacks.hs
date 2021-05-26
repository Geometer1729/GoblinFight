{-# LANGUAGE
  TemplateHaskell
#-}

module Attacks where

import Dice
import Control.Lens

import qualified Data.Map as M


type Damage = (DamageType,Dice)

data Attack = Attack{
  _bonus    :: (Int,Int,Int),
  _dmg      :: Damage,
  _ammoType :: Maybe String,
  _critDmg  :: Damage
                    }deriving Show


data DamageType =
    B
  | P
  | S
  | Fire
  | Acid
  deriving Show

type Defenses    = M.Map DamageType DefenseType
data DefenseType = Immune | Resist Int | Vuln Int

makeLenses ''Attack

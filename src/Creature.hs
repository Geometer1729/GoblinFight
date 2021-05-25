{-# LANGUAGE
  TemplateHaskell
#-}

module Creature where

import Data.Set
import Attacks
import Control.Lens

type CUID = Int
type Square = (Int,Int)

data Creature = Creature{
  _cuid                :: CUID,
  _team                :: Int,
  _hp                  :: Int,
  _ac                  :: Int,
  _initiative          :: Int,
  _speed               :: Int,
  _frightened          :: Int,
  _location            :: Square,
  _refDC               :: Int,
  _athletics           :: Int,
  _acrobatics          :: Int,
  _intimidate          :: Int,
  _attacks             :: [Attack],
  _demoralizeCooldowns :: Set CUID, -- technically only 10 minutes
  _creatureSpecific    :: CSpecific
                        } deriving Show

data CSpecific = Goblin{
  _arrows         :: Int
            }  | DEF
            deriving Show

makeLenses ''Creature
makeLenses ''CSpecific

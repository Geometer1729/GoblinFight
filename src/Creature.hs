{-# LANGUAGE
  TemplateHaskell
#-}

module Creature where

import Attacks

import Data.Set
import Control.Lens

import qualified Data.Map as M

type CUID = Int
type Square = (Int,Int)

data Creature = Creature{
  _cuid                :: CUID,
  _team                :: Int,
  _hp                  :: Int,
  _maxHp               :: Int,
  _ac                  :: Int,
  _initiative          :: Int,
  _speed               :: Int,
  _frightened          :: Int,
  _location            :: Square,
  _refDC               :: Int,
  _fortDC              :: Int,
  _willDC              :: Int,
  _athletics           :: Int,
  _acrobatics          :: Int,
  _intimidate          :: Int,
  _attacks             :: [Attack],
  _prone               :: Bool,
  _demoralizeCooldowns :: Set CUID, -- technically only 10 minutes
  _ammo                :: M.Map String Int,
  _defenses            :: Defenses,
  _unarmed             :: Int, -- unarmed attack is needed for escape checks
  _grappledBy          :: Maybe (CUID,Int), -- creature and escape DC
  _creatureSpecific    :: CSpecific
                        } deriving Show

data CSpecific = Goblin
              | Def
            deriving Show

makeLenses ''Creature
makeLenses ''CSpecific

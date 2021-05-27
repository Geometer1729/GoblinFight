{-# LANGUAGE
   TemplateHaskell
  ,RankNTypes
#-}

module Types where

import Control.Lens
import Control.Monad.State
import Data.Set

import qualified Data.Map as M

type CUID = Int
type Damage = (DamageType,Dice)
type Defenses    = M.Map DamageType DefenseType
type PF2E = StateT World IO
type Square = (Int,Int)
type Stat = Lens' Creature Int

data Action =
    Move {moveActions :: Int , movePath :: [Square] }
  | Step {stepDest :: Square }
  | Strike { strikeIndex :: Int, strikeTarget :: Square }
  | DropProne
  | Stand
  | Escape
  | Grapple{ grapTarget :: Square }
  | Demoralize{ demoralizeTarget :: Square }
  deriving Show

data AI = Native (World -> Action)
        | Executable FilePath
        | CLI -- handle through the shell the program was launched from

data Attack = Attack{
  _bonus    :: (Int,Int,Int),
  _dmg      :: Damage,
  _ammoType :: Maybe String,
  _critDmg  :: Damage
                    }deriving Show

data CheckRes = CritFail | Fail | Suc | CritSuc deriving(Enum)

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
  _ref                 :: Int,
  _fort                :: Int,
  _will                :: Int,
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

data DamageType =
    B
  | P
  | S
  | Fire
  | Acid
  deriving (Eq,Ord,Show)

data DefenseType = Immune | Resist Int | Vuln Int deriving Show

data Dice = D Int | C Int | NOf Int Dice | Add Dice Dice | Mul Dice Dice | Sub Dice Dice deriving Show

data World = World{
   _squares         :: M.Map Square CUID,
   _cresById        :: M.Map CUID Creature,
   _globalInititive :: [CUID],
   _nextCuid        :: CUID,
   _actionsLeft     :: Int,
   _ais             :: M.Map Int AI, -- maps teams to AIs
   _mapen           :: Int  -- 0 1 or 2 (rather than 0 5 or 10)
    }

makeLenses ''Attack
makeLenses ''CSpecific
makeLenses ''Creature
makeLenses ''World

instance Show World where
  show w = unlines [ "World state:"
                   , "Squares:"             ++ show ( w^.squares         )
                   , "IDLookup:"            ++ show ( w^.cresById        )
                   , "Inititive:"           ++ show ( w^.globalInititive )
                   , "actions left:"        ++ show ( w^.actionsLeft     )
                   , "multi attack penalty" ++ show ( w^.mapen           ) ]

modToDC :: Lens' a Int -> Lens' a Int
modToDC l = l . lens (+10) (\x _ -> x-10)

refDC,fortDC,willDC :: Lens' Creature Int
refDC  = modToDC ref
fortDC = modToDC fort
willDC = modToDC will


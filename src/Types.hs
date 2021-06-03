{-# LANGUAGE
   DeriveAnyClass
  ,DeriveFunctor
  ,DeriveGeneric
  ,ExistentialQuantification
  ,RankNTypes
  ,StandaloneDeriving
  ,TemplateHaskell
#-}

module Types where

import Control.Concurrent
import Control.DeepSeq
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Free
import Data.Set
import GHC.Generics
import Graphics.Gloss

import qualified Data.Map as M
import qualified Data.Set as S

-- ASync types

type Async = FreeT MCF IO
data MCF a = forall c. MCF (MVar c) (c -> a)
deriving instance Functor MCF
-- mvar continuations functor
type MAsync = ReaderT Bool Async



type CUID = Int
type Damage = (DamageType,Dice)
type Defenses    = M.Map DamageType DefenseType
type PF2E = StateT World (ReaderT (Maybe (MVar World)) MAsync)
type Square = (Int,Int)
type Stat = Lens' Creature Int

data Action =
    Move {movePath :: [Square] }
  | Step {stepDest :: Square }
  | Strike { strikeIndex :: Int, strikeTarget :: Square }
  | DropProne
  | Stand
  | Escape
  | Grapple{ grapTarget :: Square }
  | Release
  | Demoralize{ demoralizeTarget :: Square }
  deriving (Generic,NFData,Show)

data AI = Native (World -> Action) (CUID -> ReactionTrigger -> World -> Maybe Action)
        | Executable FilePath
        | CLI -- handle through the shell the program was launched from
        | Gloss -- handled through gloss

data Attack = Attack{
  _bonus    :: (Int,Int,Int),
  _dmg      :: Damage,
  _ammoType :: Maybe String,
  _critDmg  :: Damage,
  _range    :: Range
                    }deriving Show

data CheckRes = CritFail | Fail | Suc | CritSuc deriving(Enum,Eq)

data CoverLevel = None | Lesser | Standard | Greater | NoLOE deriving (Eq,Ord,Show)

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
  _grappledBy          :: Maybe (Bool,CUID), -- True indicates grapple was a crit succes
  _grappling           :: Maybe (Bool,CUID),
  _reaction            :: Bool,
  _reactions           :: M.Map ReactionTrigger [Reaction] ,
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

data PreWorld = PreWorld {
  _cres  :: [(Int,[(Creature,Square)])],
  _rects :: [(Square,Square)],
  _sqs   :: [Square]
                         } deriving Show

data Range = Simple Int | Increment Int deriving Show

data ReactionTrigger = EndMovementAdjacentAlly deriving (Eq,Ord,Show)

data ReactionEvent  = EndMovement Square Int -- space team
  deriving Show

data Reaction =
    RStep
  | RStrike
  deriving Show

data RenderData = RenderData {
                    _world       :: World,
                    _worldAsync  :: Maybe (Async World,MVar World),
                    _grassPic    :: Picture,
                    _gobPic      :: Picture,
                    _selectPic   :: Picture,
                    _screenWidth :: Int,
                    _screenHeight :: Int,
                    _globalZoom  :: Int,
                    _globalPan   :: (Float,Float),
                    _defaultImageSize :: Int,
                    _leftMouseDown :: Bool,
                    _lastDragPos :: (Float,Float),
                    _selectedSquare :: Maybe Square
                  }

data World = World{
   _squares         :: M.Map Square CUID,
   _battlefield     :: S.Set Square,
   _cresById        :: M.Map CUID Creature,
   _initTracker     :: [CUID],
   _nextCuid        :: CUID,
   _actionsLeft     :: Int,
   _mapen           :: Int,  -- 0 1 or 2 (rather than 0 5 or 10)
   _ais             :: M.Map Int AI, -- maps teams to AIs
   _aiActionAwait   :: Maybe (MVar Action),
   _glossTurn       :: Bool
    }

makeLenses ''World
makeLenses ''Attack
makeLenses ''CSpecific
makeLenses ''Creature
makeLenses ''PreWorld
makeLenses ''RenderData

instance Show World where
  show w = unlines [ "World state:"
                   , "Squares: "              ++ show ( w^.squares         )
                   , "IDLookup: "             ++ show ( w^.cresById        )
                   , "Inititive: "            ++ show ( w^.initTracker     )
                   , "actions left: "         ++ show ( w^.actionsLeft     )
                   , "multi attack penalty: " ++ show ( w^.mapen           ) ]

modToDC :: Lens' a Int -> Lens' a Int
modToDC l = l . lens (+10) (\x _ -> x-10)

refDC,fortDC,willDC :: Lens' Creature Int
refDC  = modToDC ref
fortDC = modToDC fort
willDC = modToDC will

addPair :: (Num a) => (a,a) -> (a,a) -> (a,a)
addPair (x,y) (w,z) = (x+w,y+z)
negPair :: (Num a) => (a,a) -> (a,a)
negPair (x,y) = (-x,-y)

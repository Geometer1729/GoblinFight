module Reactions where

import Types
import DistUtils

import {-# SOURCE #-} Run

import ListT
import Control.Monad.Trans
import Control.Monad
import Control.Lens

trigger :: ReactionEvent -> PF2E ()
trigger event = void . toList $ do
  (rt,predicate) <- fromFoldable $ provokes event
  cre <- lift (use cresById) >>= fromFoldable
  guard $ predicate cre
  lift $ offerReaction (cre ^. cuid) rt

provokes :: ReactionEvent -> [(ReactionTrigger,Creature -> Bool)]
provokes (EndMovement sq t) = [(EndMovementAdjacentAlly,
                  \c -> c ^. team == t && neighbor (c ^. location) sq )]

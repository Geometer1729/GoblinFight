module Run where

import Types
import Actions
import AI

import Control.Lens
import Control.Monad.State

step :: PF2E ()
step = do
  left <- use actionsLeft
  if left == 0
     then stepInit
     else runAction

stepInit :: PF2E ()
stepInit = do
  globalInititive %= rotate
  actionsLeft .= 3 -- will need to check for haste eventually
  mapen .= 0

rotate :: [a] -> [a]
rotate [] = [] -- might run if all creatures dead
rotate (x:xs) = xs ++ [x]

runAction :: PF2E ()
runAction = do
   cid <- head <$> use globalInititive
   cre <- lookupCre cid
   let teamUp = cre ^. team
   Just aiUp <- use $ ais . at teamUp
   act <- get >>= runAI aiUp
   doAction cid act



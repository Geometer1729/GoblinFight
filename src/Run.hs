{-# LANGUAGE LambdaCase #-}
module Run where

import ActionParser ()
import Actions
import Types

import Control.Concurrent
import Control.Lens hiding ((.>))
import Control.Monad.State
import Control.Applicative
import Control.DeepSeq
import Data.List
import Flow
import System.Process

import qualified Data.Map as M

step :: PF2E ()
step = do
  left <- use actionsLeft
  if left == 0
     then endOfTurn >> stepInit
     else runAction

endOfTurn :: PF2E ()
endOfTurn = do
  cresById . each . frightened %= (`subtract` 1) .> max 0

stepInit :: PF2E ()
stepInit = do
  initTracker %= rotate
  actionsLeft .= 3 -- will need to check for haste eventually
  mapen .= 0

rotate :: [a] -> [a]
rotate [] = [] -- might run if all creatures dead
rotate (x:xs) = xs ++ [x]

runAction :: PF2E ()
runAction =
  use aiActionAwait >>= \case
      Just mvar ->
        lift (tryTakeMVar mvar) >>= \case
          Just action -> do
            glossTurn .= False
            lift $ putStrLn "Action played:"
            lift $ print action
            cid <- head <$> use initTracker
            doAction cid action
            aiActionAwait .= Nothing
          Nothing -> return ()
      Nothing -> do
       cid <- head <$> use initTracker
       cre <- lookupCre cid
       let teamUp = cre ^. team
       Just aiUp <- use $ ais . at teamUp
       runAI aiUp
       lift $ putStrLn "awaiting player"

runAI :: AI -> PF2E ()
runAI (Native f) = do
  w <- get
  mvar <- lift newEmptyMVar
  let result = f w
  _ <- lift $ forkIO $ result `deepseq` putMVar mvar result
  aiActionAwait .= Just mvar
runAI CLI = do
  w <- get
  lift $ print w
  mvar <- lift newEmptyMVar
  _ <- lift $ forkIO $ readLn >>= putMVar mvar
  aiActionAwait .= Just mvar
runAI (Executable path) = do
  w <- get
  mvar <- lift newEmptyMVar
  _ <- lift $ forkIO $ readProcess path [] (show w) <&> read >>= putMVar mvar
  aiActionAwait .= Just mvar
runAI Gloss = do
  mvar <- lift newEmptyMVar
  glossTurn .= True
  aiActionAwait .= Just mvar

runAIReaction :: CUID -> ReactionTrigger -> PF2E Action
runAIReaction = undefined

offerReaction :: CUID -> ReactionTrigger -> PF2E ()
offerReaction cid rt = do
  action <- runAIReaction cid rt
  cre <- lookupCre cid
  let rs = cre ^. reactions . ix rt
  guard ( any (`validate` action) rs ) <|> error "you can't do that as a reaction"
  doAction cid action

validate :: Reaction -> Action -> Bool
validate RStep   Step{} = True
validate RStrike Strike{} = True
validate _ _ = False


detectWin :: PF2E (Maybe Int)
detectWin = do
  creatures <- use cresById
  let ts = group $ map (^. team) $ M.elems creatures
  return $ do
    guard  $ length ts == 1
    return $ head . head $ ts



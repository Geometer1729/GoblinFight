{-# LANGUAGE LambdaCase #-}
module Run where

import ActionParser
import Actions
import Types

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Lens hiding ((.>))
import Control.Monad.State
import Control.Monad.Trans
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

runAI :: AI -> PF2E ()
runAI (Native f) = do
  w <- get
  mvar <- lift newEmptyMVar
  let result = f w
  lift $ forkIO $ result `deepseq` putMVar mvar result
  aiActionAwait .= Just mvar
runAI CLI = do
  w <- get
  lift $ print w
  mvar <- lift newEmptyMVar
  lift $ forkIO $ readLn >>= putMVar mvar
  aiActionAwait .= Just mvar
runAI (Executable path) = do
  w <- get
  mvar <- lift newEmptyMVar
  lift $ forkIO $ readProcess path [] (show w) <&> read >>= putMVar mvar
  aiActionAwait .= Just mvar
runAI Gloss = do
  mvar <- lift newEmptyMVar
  glossTurn .= True
  aiActionAwait .= Just mvar

detectWin :: PF2E (Maybe Int)
detectWin = do
  cres <- use cresById
  let ts = group $ map (^. team) $ M.elems cres
  return $ do
    guard  $ length ts == 1
    return $ head . head $ ts



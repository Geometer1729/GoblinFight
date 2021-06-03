{-# LANGUAGE LambdaCase #-}
module Run where

import ActionParser ()
import Actions
import Types
import MAsync

import Control.Lens hiding ((.>))
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Control.DeepSeq
import Data.List
import Flow
import System.Process

import qualified Data.Map as M

runPF2E  :: PF2E a -> World -> IO (a,World)
runPF2E  pf2e w = forceAwaits $ runReaderT (runStateT  pf2e w) False

execPF2E :: PF2E a -> World -> IO World
execPF2E pf2e w = forceAwaits $ runReaderT (execStateT pf2e w) False

evalPF2E :: PF2E a -> World -> IO a
evalPF2E pf2e w = forceAwaits $ runReaderT (evalStateT pf2e w) False

stepPF2E :: PF2E () -> (World,Maybe (Async World)) -> IO (World,Maybe (Async World))
stepPF2E stepper (w,Nothing) =
  tryAsync stepper w >>= \case
    Left w' -> return (w',Nothing)
    Right asyncw -> return (w,Just asyncw)
stepPF2E _stepper (w,Just asyncw) =
  tryAwaits asyncw >>= \case
    Left w' -> return (w',Nothing)
    Right asyncw' -> return (w,Just asyncw')
-- this is a bit repetitive should be cleaned up

tryAsync :: PF2E () -> World -> IO (Either World (Async World))
tryAsync pf2e w = tryAwaits $ runReaderT (execStateT pf2e w) True

step :: PF2E ()
step = do
  left <- use actionsLeft
  if left == 0
     then endOfTurn >> stepInit
     else do
       w <- get
       cid <- head <$> use initTracker
       cre <- lookupCre cid
       let teamUp = cre ^. team
       Just aiUp <- use $ ais . at teamUp
       action <- lift ( masync (runAI aiUp w)) :: PF2E Action
       doAction cid action

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

runAI :: AI -> World -> IO Action
runAI (Native f _) w = let result = f w in result `deepseq` return result
runAI CLI w = do
  print w >> readLn
runAI (Executable path) w = do
  readProcess path [] (show w) <&> read
runAI Gloss _w = undefined -- gotta patch this at some point

runAIReaction :: AI -> CUID -> ReactionTrigger -> World -> IO (Maybe Action)
runAIReaction (Native _ r) cid rt w = let result = r cid rt w
                                       in result `deepseq` return result
runAIReaction CLI cid rt w = do
  print cid
  print rt
  print w
  readLn
runAIReaction (Executable fp) cid rt w = do
  readProcess fp [] (unlines [show cid,show rt,show w]) <&> read
runAIReaction Gloss _ _ _ = undefined


offerReaction :: CUID -> ReactionTrigger -> PF2E ()
offerReaction cid rt = do
  w <- get
  cre <- lookupCre cid
  Just ai <- use $ ais . at (cre ^. team)
  maction <- lift (masync (runAIReaction ai cid rt w))
  case maction of
    Just action -> do
      let rs = cre ^. reactions . ix rt
      guard ( any (`validate` action) rs ) <|> error "you can't do that as a reaction"
      doAction cid action
    Nothing -> return ()

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



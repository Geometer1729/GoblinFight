{-# LANGUAGE RankNTypes #-}

import Assets
import Types
import Display
import Control.Lens
import Run

import Control.Monad.State
import Graphics.Gloss.Interface.IO.Game

  {-
testTumbleBy :: IO World
testTumbleBy = execStateT (do
  init2Gob
  doAction 0 Move{moveActions=1,movePath=[(0,1),(0,2)]}
    ) defWorld
    -}

get2GobCLI :: IO World
get2GobCLI = execStateT (do
  init2Gob
  ais . at 1 .= Just CLI
  ais . at 2 .= Just CLI
    ) defWorld

testGraphics :: IO ()
testGraphics = do
    w <- get2GobCLI
    rd <- loadRenderData w
    print rd
    playIO (InWindow "test" (512,512) (0,0)) (makeColor 0 0 0 1) 1 rd renderAll eventHandle tick

eventHandle :: Event -> RenderData -> IO RenderData
eventHandle _event = return

tick :: Float -> RenderData -> IO RenderData
tick _duration = execStateT $ do
  w  <- use world
  w' <- lift $ execStateT step w
  world .= w'

main :: IO ()
main = do
    putStrLn "graphics test"
    testGraphics

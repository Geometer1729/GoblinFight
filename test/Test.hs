import Assets
import Types
import Actions
import Display

import Control.Monad.State
import Graphics.Gloss.Interface.IO.Game

testTumbleBy :: IO World
testTumbleBy = execStateT (do
  init2Gob
  doAction 0 Move{moveActions=1,movePath=[(0,1),(0,2)]}
    ) defWorld

testGraphics :: IO ()
testGraphics = do
    world <- testTumbleBy
    rd <- loadRenderData world
    print rd
    playIO FullScreen (makeColor 0 0 0 1) 1 rd renderGrass (\e -> \r -> return rd) (\f -> \r -> return rd)

main :: IO ()
main = do
  --putStrLn "tumble by test"
  --replicateM_ 10 $ testTumbleBy >>= print
    putStrLn "graphics test"
    testGraphics

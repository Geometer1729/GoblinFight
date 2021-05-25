import Assets
import World
import Actions
import Display

import Control.Monad.State

testTumbleBy :: IO World
testTumbleBy = execStateT (do
  init2Gob
  doAction 0 Move{moveActions=1,movePath=[(0,1),(0,2)]}
    ) defWorld

main :: IO ()
main = do
  putStrLn "tumble by test"
  replicateM_ 10 $ testTumbleBy >>= print

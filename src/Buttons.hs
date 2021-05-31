module Buttons where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data Button a = Button{
  renderAt    :: Point,
  lowerLeft   :: Point,
  upperRight  :: Point,
  notSelected :: Picture,
  selected    :: Picture,
  buttonInfo :: a
                    }

data Zipper a = Zipper [a] a [a]

type BZ a = Zipper (Button a)

renderBZ :: BZ a -> Picture
renderBZ (Zipper l s r) = Pictures $ map notSelected (l ++ r) ++ [selected s] -- selected is rendered first in case selected buttons are bigger

interpClick :: Event -> BZ a -> Maybe a
interpClick e@(EventKey (MouseButton LeftButton) Down _ _) (Zipper l s r) = catMaybes $ mapM (interpClickButton e) (l ++ [s] ++ r)
interpClick _ _ = Nothing

interpClickButton :: Event -> Button a -> Maybe a
interpClickButton (EventKey (MouseButton LeftButton) Down _ pt) b
  | pt `inRect` (lowerLeft b,upperRight b) = Just $ buttonInfo b
interpClickButton _ _ = Nothing

inRect :: Point -> (Point,Point) -> Bool
inRect (x,y) ((x1,y1),(x2,y2)) = and [x1<x,x<x2,y1<y,y<y2]



module Buttons where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe

data Button a = Button{
  buttonPic    :: Picture,
  buttonInfo :: a
}

data Zipper a = Zipper [a] a [a]

type BZ a = Zipper (Button a)

renderBZ :: BZ a -> Picture
renderBZ (Zipper l s r) = Pictures $ map buttonPic (l ++ r) ++ [buttonPic s] -- selected is rendered first in case selected buttons are bigger

{-
interpClick :: Event -> BZ a -> Maybe a
interpClick e@(EventKey (MouseButton LeftButton) Down _ _) (Zipper l s r) = listToMaybe . catMaybes $ (interpClickButton e) <$> (l ++ [s] ++ r)
interpClick _ _ = Nothing

interpClickButton :: Event -> Button a -> Maybe a
interpClickButton (EventKey (MouseButton LeftButton) Down _ pt) b
  | pt `inRect` (lowerLeft b,upperRight b) = Just $ buttonInfo b
interpClickButton _ _ = Nothing
-}

inRect :: Point -> (Point,Point) -> Bool
inRect (x,y) ((x1,y1),(x2,y2)) = and [x1<x,x<x2,y1<y,y<y2]

zipForward :: Zipper a -> Zipper a
zipForward z@(Zipper l s []) = z
zipForward (Zipper l s (r:rs)) = Zipper (s:l) r rs

zipBackward :: Zipper a -> Zipper a
zipBackward z@(Zipper [] s r) = z
zipBackward (Zipper (l:ls) s r) = Zipper ls l (s:r)

getButtonImage :: Float -> Float -> Color -> String -> Picture
getButtonImage width height c t =
    let base = Color c $ rectangleSolid width height
        lightTint = Translate 0 (4*height/10)  $ Color (bright c) $ rectangleSolid width (height/5)
        darkTint  = Translate 0 (-4*height/10) $ Color (dim c)    $ rectangleSolid width (height/5)
        text = Text t
    in Pictures [base, lightTint, darkTint, text]

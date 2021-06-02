module DistUtils where

import Types

linf :: Square -> Square -> Int
linf (x1,y1) (x2,y2) = 5 * max (abs (x1-x2)) (abs (y1-y2))

neighbor :: Square -> Square -> Bool
neighbor a b = linf a b == 5

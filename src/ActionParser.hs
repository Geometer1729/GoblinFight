module ActionParser where

import Types
import Text.ParserCombinators.ReadP hiding (many,optional)
import Data.Char
import Control.Applicative
import Data.Functor

instance Read Action where
  readsPrec _ = readP_to_S actionParser

moveParser :: ReadP Action
moveParser = Move <$> (string "move" *> skipSpaces *> parseInt) <*> (skipSpaces *> parsePath)

stepParser :: ReadP Action
stepParser = Step <$> (string "step" *> skipSpaces *> parseSq)

strikeParser :: ReadP Action
strikeParser = Strike <$> (string "strike" *> skipSpaces *> option 1 parseInt) <*> (skipSpaces *> parseSq)

dropProneParser :: ReadP Action
dropProneParser = string "drop" $> DropProne

standParser :: ReadP Action
standParser = string "stand" $> Stand

escapeParser :: ReadP Action
escapeParser = string "escape" $> Escape

grappleParser :: ReadP Action
grappleParser = Grapple <$> (string "grapple" *> skipSpaces *> parseSq)

demoralizeParser :: ReadP Action
demoralizeParser = Demoralize <$> (string "demoralize" *> skipSpaces *> parseSq)

actionParser :: ReadP Action
actionParser = choice [moveParser,stepParser,strikeParser,dropProneParser,standParser,escapeParser,grappleParser,demoralizeParser]

parseInt :: ReadP Int
parseInt = read <$> munch isDigit

parseSq :: ReadP Square
parseSq = (,) <$> (optional (char '(') *> parseInt <* char ',') <*> (parseInt <* optional (char ')') )

parsePath :: ReadP [Square]
parsePath = (string "[]" $> []) +++ (do
  x <- char '[' *> parseSq
  xs <- many $ char ',' *> parseSq
  _ <- char ']'
  return $ x:xs
    )

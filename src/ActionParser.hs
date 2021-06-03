{-# LANGUAGE LambdaCase #-}

module ActionParser where

import Types
import Text.ParserCombinators.ReadP hiding (many,optional)
import Data.Char
import Control.Applicative
import Data.Functor

import Debug.Trace

instance Read Action where
  readsPrec _ w = traceShow w $ readP_to_S actionParser w

moveParser :: ReadP Action
moveParser = Move <$> ( string "move" *> skipSpaces *> parsePath )

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

releaseParser :: ReadP Action
releaseParser = string "release" $> Release

demoralizeParser :: ReadP Action
demoralizeParser = Demoralize <$> (string "demoralize" *> skipSpaces *> parseSq)

actionParser :: ReadP Action
actionParser = skipSpaces *> choice [moveParser,stepParser,strikeParser,dropProneParser,standParser,escapeParser,grappleParser,releaseParser,demoralizeParser]

parseInt :: ReadP Int
parseInt = munch isDigit >>= \case
  [] -> pfail
  ds -> return $ read ds

parseSq :: ReadP Square
parseSq = (,) <$> (optional (char '(') *> parseInt <* char ',') <*> (parseInt <* optional (char ')') )

parsePath :: ReadP [Square]
parsePath = (string "[]" $> []) +++ (do
  x <- char '[' *> parseSq
  xs <- many $ char ',' *> parseSq
  _ <- char ']'
  return $ x:xs
    )

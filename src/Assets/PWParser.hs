module Assets.PWParser where

import ActionParser
import Assets.Defs
import Types

import Data.Functor
import Flow
import Text.ParserCombinators.ReadP hiding (many)
import Control.Applicative

parsePw :: String -> PreWorld
parsePw = readP_to_S pwParser .> filter (snd .> null) .> head .> fst

pwParser :: ReadP PreWorld
pwParser = do
  _ <- string "teams\n"
  cres' <- parseTeams
  _ <- string "rects\n"
  rects' <- many $ (,) <$> (parseSq <* char ' ') <*> (skipSpaces *> parseSq <* char '\n')
  _ <- string "squares\n"
  sqs' <- many $ parseSq <* char '\n'
  return $ PreWorld cres' rects' sqs'

parseTeams :: ReadP [(Int,[(Creature,Square)])]
parseTeams = many parseTeam

parseTeam :: ReadP (Int,[(Creature,Square)])
parseTeam = (,) <$> (parseInt <* char '\n') <*> parseCres

parseCres :: ReadP [(Creature,Square)]
parseCres = many parseCre

parseCre :: ReadP (Creature,Square)
parseCre = (,) <$> (skipSpaces *> parseJustCre <* char ' ') <*> (skipSpaces *> parseSq <* char '\n')

parseJustCre :: ReadP Creature
parseJustCre = choice [
  string "gob" $> defGob
                      ]


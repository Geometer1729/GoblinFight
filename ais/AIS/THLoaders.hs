module AIS.THLoaders where

import System.Directory
import Language.Haskell.TH
import Data.Char
import Data.Maybe

loadNatives :: Q Exp
loadNatives = do
  modules' <-  runIO $ listDirectory "./ais/AIS/Natives"
  let modules = takeWhile (/= '.') <$> modules'
  exps <- catMaybes <$> mapM loadNative modules
  return $ ListE exps

loadNative :: String -> Q (Maybe Exp)
loadNative mod = do
  let varName = let (c:cs) = mod in toLower c : cs
  maybeName <- lookupValueName ("AIS.Natives." ++ mod ++ "." ++ varName)
  case maybeName of
    Nothing -> return Nothing
    Just name -> return $ Just $ TupE
      [Just (LitE (StringL varName)),
      Just (VarE name)]

loadExecs :: Q Exp
loadExecs = do
  files <-  runIO $ listDirectory "./ais/AIS/Executables"
  Just exeName <- lookupValueName "Executable"
  return $ ListE [ TupE
    [ Just (LitE (StringL file)),
      Just (AppE (VarE exeName) (LitE (StringL $ "./ais/AIS/Executables/" ++ file ))) ]
      | file <- files ]


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
loadNative modu = do
  let varName = let (c:cs) = modu in toLower c : cs
  maybeName <- lookupValueName ("AIS.Natives." ++ modu ++ "." ++ varName)
  maybeRName <- lookupValueName ("AIS.Natives." ++ modu ++ "." ++ varName ++ "React")
  case maybeName of
    Nothing -> return Nothing
    Just name -> case maybeRName of
                   Nothing -> return Nothing
                   Just rname -> return $ Just $ TupE
                          [Just (LitE (StringL varName))
                          ,Just (VarE name)
                          ,Just (VarE rname)
                          ]

loadExecs :: Q Exp
loadExecs = do
  files <-  runIO $ listDirectory "./ais/AIS/Executables"
  Just exeName <- lookupValueName "Executable"
  return $ ListE [ TupE
    [ Just (LitE (StringL file)),
      Just (AppE (ConE exeName) (LitE (StringL $ "./ais/AIS/Executables/" ++ file ))) ]
      | file <- files ]


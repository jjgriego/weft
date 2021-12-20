{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
module Module ( rules
              , link
              , pcm
              , name
              , deps
              , source
              )where

import Development.Shake
import Development.Shake.FilePath

import Text.Regex.TDFA
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Data.Foldable
import Data.Maybe
import Control.Lens

import Util

data ModuleLine = ImportMod String
  deriving Show

recognizeLines :: [String] -> [ModuleLine]
recognizeLines lines = catMaybes $ recognizeLine <$> lines 
  where
    recognizeLine line =
      let matchSubgroups pat =
            (\(_, _, _, submatches) -> submatches)<$> (line =~~ pat :: Maybe (String, String, String, [String])) in
        asum [ ImportMod <$> head <$> matchSubgroups "^import ([a-zA-Z0-9_-]*);$"
             ]

sourceOfMod x = addExtension ("src" </> x) ".cpp"

modOfSource src = dropExtension $ makeRelative "src" src

moduleDeps :: String -> Action [String]
moduleDeps src = do
  modLines <- recognizeLines <$> readFileLines src
  forM modLines $ \case
    ImportMod name -> return name


data ModInfo = ModInfo { _name :: String
                       , _deps :: [ModInfo]
                       , _source :: String
                       , _pcm :: String
                       } deriving (Show)
$(makeLenses ''ModInfo)

pcmPath modname = artifact (addExtension modname ".pcm")

transitiveDeps :: [ModInfo] -> [ModInfo]
transitiveDeps mods = visit mods M.empty
  where
    visit :: [ModInfo] -> M.Map String ModInfo -> [ModInfo]
    visit [] seen = M.elems seen
    visit (modinfo:more) seen
      | M.member (modinfo ^. name) seen = visit more seen
      | otherwise                       = visit (more ++ (modinfo ^. deps))
                                                (M.insert (modinfo ^. name) modinfo seen)

link :: String -> ([String] -> Action ()) -> [ModInfo] -> Action ()
link outfile cxx roots = do
  let allMods = transitiveDeps roots
  let pcms = ((^.pcm) <$> allMods) 
  putVerbose ("module list: " ++ (show pcms))
  need pcms
  putVerbose ("linking modules")
  cxx ([ "-o", outfile ] ++ pcms)

rules cxx = do
  rec { modInfo <-
        newCache (\sourceFile -> do
                     putVerbose ("reading module: " ++ sourceFile)
                     let modname = modOfSource sourceFile
                     deps <- moduleDeps sourceFile
                     depsInfo <- forM deps (modInfo . sourceOfMod)
                     return $ ModInfo modname depsInfo sourceFile (pcmPath modname)) }

  artifact "*.pcm" %> \art -> do
    let modname = dropExtension $ artifactName art
    let src = sourceOfMod $ modname
    info <- modInfo src
    let depMods = (info ^. deps)
    let pcmPaths = (^.pcm) <$> depMods
    need (pcmPaths ++ [src])
    cxx ([ "-Xclang"
          , "-emit-module-interface"
          , "-c", src
          , "-o", pcmPath modname
          ] ++
          ["-fmodule-file=" ++ mod ++ "=" ++ "/home/jgriego/proj/weft/" ++ pcm
          | ModInfo { _name = mod } <- depMods
          , let pcm = pcmPath mod
          ])

  return modInfo

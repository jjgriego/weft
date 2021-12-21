{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Module ( moduleObj
              )where

import Development.Shake
import Development.Shake.FilePath

import Text.Regex.TDFA
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Data.Foldable
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import Control.Lens
import Data.Fix
import Data.List.Split
import Data.List

import Util

data ModuleLine = ImportMod String
                | BeginPrivateFragment
                | ModuleDecl Bool
  deriving Show

resolveName context name
  | name =~ "^:" = context ++ name
  | otherwise    = name

recognizeLines :: String -> [String] -> [ModuleLine]
recognizeLines context lines = catMaybes $ recognizeLine <$> lines 
  where
    recognizeLine line =
      let matchSubgroups pat =
            (\(_, _, _, submatches) -> submatches)<$> (line =~~ pat :: Maybe (String, String, String, [String])) in
        asum [ ImportMod <$> resolveName context <$> (!!1) <$> matchSubgroups "^(export )?import ([a-zA-Z0-9_.:-]*);$"
             , const BeginPrivateFragment                  <$> matchSubgroups "^module :private;$"
             , ModuleDecl <$> not . null <$> head          <$> matchSubgroups "^(export )?module ([a-zA-Z0-9_.:-]*);$"
             ]

primarySourceOfMod x = addExtension ("src" </> x) ".cpp"

modBaseName modname = last $ splitOn "." modname

listImplementationUnits mod = do
  let dir = takeDirectory $ primarySourceOfMod mod
  filenames <- getDirectoryFiles dir [modBaseName mod ++ "-*.cpp"]
  return ((dir </>) <$> filenames)


modOfSource src = dropImplSuffix $ fmap dirToDots $ dropExtension $ makeRelative "src" src
  where
    dirToDots c = c

    dropImplSuffix name = head $ splitOn "-" name

data UnitType = TPrimary | TAux deriving (Show, Eq, Ord)

data UnitInfo m = UnitInfo { _deps :: [m]
                           , _source :: String
                           , _unitType :: UnitType
                           } deriving (Show, Functor, Foldable, Traversable)
$(makeLenses ''UnitInfo)

instance Eq (UnitInfo m) where
  m1 == m2 = (m1 ^. source) == (m2 ^. source)
instance Ord (UnitInfo m) where
  compare m1 m2 = compare (m1 ^. source) (m2 ^. source)

data ModInfo m = ModInfo { _modName :: String
                         , _primaryUnit :: UnitInfo m
                         , _otherUnits :: [UnitInfo m]
                         } deriving (Show, Functor, Foldable, Traversable)
$(makeLenses ''ModInfo)

instance Eq (ModInfo m) where
  m1 == m2 = (m1 ^. modName) == (m2 ^. modName)
instance Ord (ModInfo m) where
  compare m1 m2 = compare (m1 ^. modName) (m2 ^. modName)


pcmPath unitSrc = artifact (makeRelative "src" $ replaceExtension unitSrc ".pcm")
objPath unitSrc = artifact (makeRelative "src" $ addExtension unitSrc ".o")
unitArtifact u
  | (u ^. unitType) == TPrimary = pcmPath (u ^. source)
  | otherwise                   = objPath (u ^. source)


minimalCycle (x:xs) = (x : takeWhile (/= x) xs) ++ [x]

enumerateUnits :: (String -> Action (ModInfo String))
               -> [String] -> Action [UnitInfo (Fix ModInfo)]
enumerateUnits modInfo mods = do
    allMods <- S.toList <$> execStateT (runReaderT (mapM checkMod mods) []) (S.empty)
    fixer <- makeFixer allMods
    return $ S.toList $ execState (forM (fixer <$> allMods) visitMod) S.empty
  where
    -- visit a unit and chase any modules that are needed to compile its
    -- interface unit, asserting that they do not form any cycle
    checkMod :: String -> ReaderT [String] (StateT (S.Set String) Action) ()
    checkMod modname = do
      because <- ask
      when (elem modname because) (fail ("cycle in module interfaces: " ++ (concat $ intersperse " -> " $ reverse $ minimalCycle $ (modname:because))))
      local (modname:) $ do
        -- we visit the primary interface unit immediately--the others need to
        -- be included but can be deferred
        mod <- lift $ lift $ modInfo modname
        forM (mod ^. primaryUnit . deps) checkMod
      modify $ S.insert modname

    makeFixer :: [String] -> Action (String -> (Fix ModInfo))
    makeFixer allNames = do
      retrieved <- forM allNames $ \name -> do
        mod <- modInfo name
        return (name, mod)

      let findInM name = fromJust $ M.lookup name m
          link (name, mod) = (name, Fix (findInM <$> mod))
          m = M.fromList (link <$> retrieved) in
        return findInM

    visitMod (Fix m) = do
      visitUnit (m ^. primaryUnit)
      forM_ (m ^. otherUnits) visitUnit

    visitUnit u = do
      seen <- gets (S.member u)
      when (not seen) $ do
        modify (S.insert u)
        forM_ (u ^. deps) visitMod

data ModParseState = ModParseState { _psPrimary :: Bool
                                   , _psDeps    :: [String]}
$(makeLenses ''ModParseState)

moduleObj :: ([String] -> Action ()) -> ([String] -> Action ()) -> String -> [String] -> Rules ()
moduleObj cxx ld obj roots = do
  unitInfo <- newCache (\src -> do
                          let context = modOfSource src
                          modLines <- recognizeLines context <$> readFileLines src
                          final <- flip execStateT (ModParseState False []) $ forM modLines $ \case
                            ModuleDecl primary -> psPrimary .= primary
                            ImportMod name -> psDeps %= (name:)
                            BeginPrivateFragment -> return ()
                          let deps = final ^. psDeps
                          let (typ, addnlDeps) = if (final ^. psPrimary)
                                                 then (TPrimary, [])
                                                 else (TAux, [context])
                          return $ UnitInfo (deps ++ addnlDeps) src typ)

  rec { modInfo <- newCache (\modname -> do
                                putVerbose ("\nmodule: " ++ modname)
                                let src = primarySourceOfMod modname
                                implSrcs <- listImplementationUnits modname
                                primary <- unitInfo src
                                when ((primary ^. unitType) /= TPrimary) $ do
                                  fail ("primary interface unit does not begin with `export module " ++ modname ++ ";`")
                                otherUnits <- mapM unitInfo implSrcs
                                putVerbose ("  -> primary unit: " ++ (primary ^. source))
                                when (not (null (primary ^. deps))) $ putVerbose ("          depends: " ++ (concat $ intersperse ", " (primary ^. deps)))
                                forM otherUnits $ \s -> do
                                  putVerbose ("  ->         unit: " ++ (s ^. source))
                                  when (not (null (s ^. deps))) $ putVerbose ("          depends: " ++ (concat $ intersperse ", " (s ^. deps)))
                                return $ ModInfo modname primary otherUnits) }

  let modules_cxx src addnlFlags = do
        info <- unitInfo src
        depMods <- mapM modInfo (info ^. deps)
        let pcmPaths = pcmPath <$> (^.primaryUnit.source) <$> depMods
        need ([src] ++ pcmPaths)
        putVerbose (show (src, pcmPaths))
        cxx (addnlFlags ++
             ["-fmodule-file=" ++ modname ++ "=" ++ pcmFile
             | mod <- depMods
             , let modname = mod ^. modName
             , let pcmFile = pcmPath (mod ^. primaryUnit . source)
             ] ++
             [ -- "-fprebuilt-module-path=build/out"
              --"-fimplicit-modules"
             ])

  artifact "*.pcm" %> \out -> do
    let src = "src" </> (replaceExtensions (artifactName out) ".cpp")
    modules_cxx src [ --"-Xclang" , "-emit-module-interface"
                    , "-c", src
                    , "-o", out
                    ]

  artifact "*.cpp.o" %> \out -> do
    let src = "src" </> (replaceExtensions (artifactName out) ".cpp")
    modules_cxx src [ "-c", src
                    , "-o", out
                    ]

  obj %> \out -> do
    putVerbose "ahh"
    allUnits <- enumerateUnits modInfo roots
    putVerbose "nope"
    let pcms = (unitArtifact <$> allUnits)
    putVerbose ("unit list: " ++ (show pcms))
    need pcms
    putVerbose ("linking modules")
    cxx ([ "-o", out ] ++ pcms)

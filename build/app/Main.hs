{-# LANGUAGE TypeFamilies #-}
module Main where

import Development.Shake
import Development.Shake.FilePath
import System.Directory

import Util
import qualified Module as M

getSourceFiles = readFileLines "src/Sources.txt"

main :: IO ()
main = do
  rootPath <- head . lines . fromStdout <$> cmd "git rev-parse --show-toplevel"
  withCurrentDirectory rootPath $ shakeArgs shakeOptions $ do
    want ["build/out/dist/weft.iso"]
    action $ cmd_ "mkdir -p build/out"

    phony "clean" $ removeFilesAfter "build/out" ["//"]

    M.moduleObj cxx ld (artifact "weft.x86_64.elf") ["entry"]
    -- (artifact "weft.x86_64.elf") %> \out -> do
      -- roots <- mapM lookupMod ["entry"]
      -- M.link out (addFlags cxx [ "-T", "kernel.ld"
                               -- -- , "-fwhole-program"
                               -- ]) roots

    (artifact "dist/weft.iso")%> \out -> do
      let kernel_exe = "build/out/weft.x86_64.elf"
      let mkbootimg_config = "mkbootimg.json"
      let bootboot_config = "config.cfg"
      need ["build/out/weft.x86_64.elf",  mkbootimg_config,  bootboot_config]
      cmd_ "mkdir -p build/out/boot"
      cmd_ "mkdir -p build/out/dist"
      cmd_ "cp" [kernel_exe] "build/out/boot"
      cmd_ "cp" [mkbootimg_config] "build/out/"
      cmd_ "cp" [bootboot_config] "build/out/"
      command_ [Cwd "build/out"] "mkbootimg" [ mkbootimg_config
                                            , artifactName out
                                            ]

  where
    commandBase opts str flags addnlFlags = command_ opts str (flags ++ addnlFlags)
    addFlags f flags = (\addnlFlags -> f (flags ++ addnlFlags))

    cxx additionalFlags = do
      envFlags <- words <$> maybe "" id <$> getEnv "NIX_CFLAGS_COMPILE"
      command_ [] "x86_64-elf-gcc" (["-std=c++20"
                                    , "-fmodules-ts"
                           , "-ffreestanding"
                           , "-mcmodel=large"
                           , "-fdiagnostics-color=always"
                           , "-mno-red-zone"
                           , "-mno-mmx"
                           , "-mno-sse"
                           , "-O2"
                           , "-fdiagnostics-color=always"
                           , "-mno-sse2"
                           , "-fpic"
                           , "-fno-stack-protector"
                           , "-fno-exceptions"
                           , "-fno-rtti"
                           , "-nostdlib"
                           ] ++ envFlags ++ additionalFlags)

    ld additionalFlags = do
      need ["kernel.ld"]
      cxx (["-T", "kernel.ld"] ++ additionalFlags)
      

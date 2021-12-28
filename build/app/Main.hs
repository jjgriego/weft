module Main where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory
import Control.Monad

artifact path = "build/out/" ++ path
artifactName path = makeRelative "build/out" path
cxx addnlArgs = do
  command [] "clang" ([
                        "--target=x86_64-unknown-none-elf"
                      , "-std=c++20"
                      , "-ffreestanding"
                      , "-mcmodel=large"
                      , "-mno-red-zone"
                      , "-mno-mmx"
                      , "-mno-sse"
                      , "-O2"
                      , "-mno-sse2"
                      , "-fno-stack-protector"
                      , "-fno-exceptions"
                      , "-fno-rtti"
                      , "-fpic"
                      , "-fdiagnostics-color=always"
                      , "-nostdlib"
                      , "-flto"
                      , "-Werror"
                      , "-Weverything"
                      , "-Wno-padded"
                      , "-Wconversion"
                      , "-Wno-c++98-compat"
                      , "-Wconversion"
                      ] ++ addnlArgs)
cxx_ args = cxx args :: Action ()

main :: IO ()
main = do
  rootPath <- head . lines . fromStdout <$> cmd "git rev-parse --show-toplevel"
  withCurrentDirectory rootPath $ shakeArgs shakeOptions $ do
    want ["build/out/dist/weft.iso"]
    action $ cmd_ "mkdir -p build/out"

    phony "clean" $ removeFilesAfter "build/out" ["//"]

    -- "build/out" %> cmd_ "mkdir -p build/out"
    (artifact "*.cpp.o") %> \outfile -> do
      let name = dropExtension $ artifactName outfile
      let depsfile = artifact (addExtension name ".d")
      let infile = "src" </> name
      nixflags <- maybe [] words <$> getEnv "NIX_CFLAGS_COMPILE"
      need [infile, depsfile]
      needMakefileDependencies depsfile
      cxx (nixflags ++ [ "-c", infile , "-o", outfile ])

    (artifact "*.cpp.d") %> \outfile -> do
      let infile = "src" </> (dropExtension $ artifactName outfile)
      need [infile]
      nixflags <- maybe [] words <$> getEnv "NIX_CFLAGS_COMPILE"
      cxx_ (nixflags ++ ["-c", infile, "-M", "-MF", outfile, "-MMD", "-o", "/dev/null"])
      needMakefileDependencies outfile

    (artifact "weft.x86_64.elf") %> \out -> do
      kernelObjects <- fmap (("build/out" </>) . flip addExtension ".o") <$> filter ("*.cpp"?==) <$> readFileLines "src/Sources.txt"
      need (kernelObjects ++ ["kernel.ld"])
      cxx_ ([ "-nostdlib"
            , "-nostartfiles"
            , "-fuse-ld=lld"
            , "-fwhole-program"
            , "-T", "kernel.ld"
            , "-o", out]
             ++ kernelObjects)

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

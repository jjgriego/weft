module Main where

import Development.Shake
import Development.Shake.FilePath
import System.Directory

artifact path = "build/out/" ++ path
artifactName path = makeRelative "build/out" path

cpp_compile outfile = do
  let infile = "src" </> (dropExtension $ artifactName outfile)
  need [infile]
  command_ [] "x86_64-elf-gcc" [ "-c", infile
                               , "-o", outfile
                               , "-std=c++20"
                               , "-ffreestanding"
                               , "-mcmodel=large"
                               , "-mno-red-zone"
                               , "-mno-mmx"
                               , "-mno-sse"
                               , "-O2"
                               , "-mno-sse2"
                               , "-fpic"
                               , "-fno-stack-protector"
                               , "-fno-exceptions"
                               , "-fno-rtti"
                               , "-fdiagnostics-color=always"
                               -- , "-nostdinc"
                               , "-nostdlib"
                               ]

kernelObjects = (artifact . flip addExtension ".o") <$>
  [ "entry.cpp"
  ]



main :: IO ()
main = do
  rootPath <- head . lines . fromStdout <$> cmd "git rev-parse --show-toplevel"
  withCurrentDirectory rootPath $ shakeArgs shakeOptions $ do
    want ["build/out/dist/weft.iso"]
    action $ cmd_ "mkdir -p build/out"

    phony "clean" $ removeFilesAfter "build/out" ["//"]

    -- "build/out" %> cmd_ "mkdir -p build/out"
    (artifact "*.cpp.o") %> cpp_compile

    (artifact "weft.x86_64.elf") %> \out -> do
      kernelObjects <- fmap (("build/out" </>) . flip addExtension ".o") <$> filter ("*.cpp"?==) <$> readFileLines "src/Sources.txt"
      need (kernelObjects ++ ["kernel.ld"])
      command_ [] "x86_64-elf-ld" ([ "-nostdlib"
                                    , "-nostartfiles"
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

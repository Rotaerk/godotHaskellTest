module Main where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs options $ do
  want ["build"]

  "clean" ~> do
    cmd_ "cabal" ["clean"]
    liftIO $ removeFiles godotLibDirPath [libFileName]
    removeFilesAfter shakeDirPath ["//*"]

  "build" ~> do
    cmd_ "cabal" ["build"]
    copyFileChanged cabalLibFilePath godotLibFilePath

  "run" ~> do
    need ["build"]
    cmd "godot" ["--path", godotProjectDirPath]

options :: ShakeOptions
options = shakeOptions { shakeFiles = shakeDirPath }

shakeDirPath :: FilePath
shakeDirPath = ".shake"

libFileName :: FilePath
libFileName = "libsandbox.so"

cabalLibFilePath :: FilePath
cabalLibFilePath = "../dist-newstyle/build/x86_64-linux/ghc-9.2.2/sandbox-0.1.0.0/f/sandbox/build/sandbox" </> libFileName

godotProjectDirPath :: FilePath
godotProjectDirPath = "game"

godotLibDirPath :: FilePath
godotLibDirPath = godotProjectDirPath </> "lib"

godotLibFilePath :: FilePath
godotLibFilePath = godotLibDirPath </> libFileName

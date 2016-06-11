{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad
import qualified Data.ByteString.Char8 as C
import           Data.Monoid
import           Development.Shake
import           Development.Shake.FilePath

main :: IO ()
main = shakeArgs shakeOptions $ do
  want [ "hsfiles/library.hsfiles"
       , "hsfiles/private-executable.hsfiles"
       , "hsfiles/public-executable.hsfiles"
       ]
  "hsfiles/*.hsfiles" %> \out -> do
    contents <- readFileLines $ ("templates" </> dropDirectory1 out) -<.> "txt"
    need contents
    putNormal $ "Generating " ++ out
    fs <- liftIO $ forM contents $ \filename -> do
      content <- C.readFile filename
      return (dropDirectory1 filename, content)
    liftIO $ C.writeFile out $ concatHsFiles fs

concatHsFiles :: [(FilePath, C.ByteString)] -> C.ByteString
concatHsFiles = mconcat . concatMap (\(f, c) -> ["{-# START_FILE " <> C.pack f <> " #-}\n", c])

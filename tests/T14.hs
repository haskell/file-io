{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception
import System.OsPath ((</>), osp)
import qualified System.OsPath as OSP
import qualified System.File.OsPath as OSP
import System.IO.Temp

-- Test that 'readFile' does not create a file
-- https://github.com/hasufell/file-io/issues/14
main :: IO ()
main = withSystemTempDirectory "tar-test" $ \baseDir' -> do
  baseDir <- OSP.encodeFS baseDir'
  res <- try @SomeException $ OSP.readFile (baseDir </> [osp|foo|])
  case res of
    Left e -> print e >> return ()
    Right _ -> fail "Unexpectedly found file 'foo'"


{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception
import System.OsPath ((</>), osp)
import qualified System.OsPath as OSP
import qualified System.File.OsPath as OSP
import TestUtils

-- Test that 'readFile' does not create a file
-- https://github.com/hasufell/file-io/issues/14
main :: IO ()
main = withFileIOTestDir $ \baseDir -> do
  res <- try @SomeException $ OSP.readFile (baseDir </> [osp|foo|])
  case res of
    Left e -> print e >> return ()
    Right _ -> fail "Unexpectedly found file 'foo'"


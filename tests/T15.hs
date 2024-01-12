{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import System.OsPath ((</>), osp)
import qualified System.OsPath as OSP
import qualified System.File.OsPath as OSP
import System.IO
import System.IO.Temp

-- Test that we can read concurrently without file lock
-- https://github.com/hasufell/file-io/issues/15
main :: IO ()
main = withSystemTempDirectory "tar-test" $ \baseDir' -> do
  baseDir <- OSP.encodeFS baseDir'
  OSP.writeFile (baseDir </> [osp|foo|]) ""
  defaultMain $ testGroup "All"
    [ testGroup "System.File.OsPath"
    $ map (\i -> testCase ("foo " <> show i) (OSP.openFile (baseDir </> [osp|foo|]) ReadMode >>= hClose)) ([0..99] :: [Int])
    ]


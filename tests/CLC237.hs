{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception
import System.OsPath ((</>), osp)
import qualified System.OsPath as OSP
import qualified System.File.OsPath as OSP
import GHC.IO.Exception (IOErrorType(..), IOException(..))
import System.IO
import System.IO.Temp

-- Test that the action in 'withFile' does not inherit the filepath annotation
-- See https://github.com/haskell/core-libraries-committee/issues/237
main :: IO ()
main = withSystemTempDirectory "tar-test" $ \baseDir' -> do
  baseDir <- OSP.encodeFS baseDir'
  res <- try @IOException $ OSP.withFile (baseDir </> [osp|foo|]) WriteMode $ \_ -> fail "test"
  case res of
    Left (IOError Nothing UserError "" "test" Nothing Nothing) -> pure ()
    Left e -> print e >> fail "Unexpected error"
    Right _ -> fail "Unexpected success"


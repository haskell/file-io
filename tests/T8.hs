{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Concurrent
import Control.Monad
import System.File.OsPath
import System.OsPath
import System.IO.Temp

import qualified Data.ByteString.Lazy as BL
import qualified System.OsPath as OSP
import qualified System.File.OsPath as OSP

main :: IO ()
main = withSystemTempDirectory "test" $ \baseDir' -> do
    let fn = [osp|test.txt|]
    baseDir <- OSP.encodeFS baseDir'
    let fp = baseDir OSP.</> fn
    OSP.writeFile fp ""

    replicateM_ 100000 $ do
      thr <- forkIO (System.File.OsPath.readFile fp >>= BL.putStr)
      threadDelay 1
      void $ killThread thr

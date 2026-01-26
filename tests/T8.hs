{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Concurrent
import Control.Monad
import System.File.OsPath
import System.OsPath
import TestUtils

import qualified Data.ByteString.Lazy as BL
import qualified System.OsPath as OSP
import qualified System.File.OsPath as OSP

main :: IO ()
main = withFileIOTestDir $ \baseDir -> do
    let fn = [osp|test.txt|]
    let fp = baseDir OSP.</> fn
    OSP.writeFile fp ""

    replicateM_ 100000 $ do
      thr <- forkIO (System.File.OsPath.readFile fp >>= BL.putStr)
      threadDelay 1
      void $ killThread thr

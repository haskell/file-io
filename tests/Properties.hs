{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception
import Data.Bifunctor (first)
import Test.Tasty
import Test.Tasty.HUnit
import System.OsPath ((</>), osp)
import qualified System.OsPath as OSP
import qualified System.File.OsPath as OSP
import GHC.IO.Exception (IOErrorType(..), IOException(..))
import System.IO
import System.IO.Temp
import qualified Data.ByteString as BS


main :: IO ()
main = defaultMain $ testGroup "All"
    [ testGroup "System.File.OsPath"
       [ testCase "readFile . writeFile" writeFileReadFile
       , testCase "readFile . appendFile . writeFile" appendFileReadFile
       , testCase "iomode: ReadFile does not allow write" iomodeReadFile
       , testCase "iomode: WriteFile does not allow read" iomodeWriteFile
       , testCase "iomode: AppendMode does not allow read" iomodeAppendFile
       , testCase "iomode: ReadWriteMode does allow everything" iomodeAppendFile
       ]
    ]

writeFileReadFile :: IO ()
writeFileReadFile = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    OSP.writeFile (baseDir </> [osp|foo|]) "test"
    contents <- OSP.readFile (baseDir </> [osp|foo|])
    "test" @=? contents

appendFileReadFile :: IO ()
appendFileReadFile = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    OSP.writeFile (baseDir </> [osp|foo|]) "test"
    OSP.appendFile (baseDir </> [osp|foo|]) "test"
    contents <- OSP.readFile (baseDir </> [osp|foo|])
    "testtest" @=? contents

iomodeReadFile :: IO ()
iomodeReadFile = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    OSP.writeFile (baseDir </> [osp|foo|]) ""
    r <- try @IOException $ OSP.withFile (baseDir </> [osp|foo|]) ReadMode $ \h -> BS.hPut h "test"
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    Left PermissionDenied
#else
    Left IllegalOperation
#endif
      @=? first ioe_type r

iomodeWriteFile :: IO ()
iomodeWriteFile = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    OSP.writeFile (baseDir </> [osp|foo|]) ""
    r <- try @IOException $ OSP.withFile (baseDir </> [osp|foo|]) WriteMode $ \h -> BS.hGetContents h
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    Left InvalidArgument
#else
    Left IllegalOperation
#endif
      @=? first ioe_type r

iomodeAppendFile :: IO ()
iomodeAppendFile = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    OSP.writeFile (baseDir </> [osp|foo|]) ""
    r <- try @IOException $ OSP.withFile (baseDir </> [osp|foo|]) AppendMode $ \h -> BS.hGetContents h
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    Left InvalidArgument
#else
    Left IllegalOperation
#endif
      @=? first ioe_type r

iomodeReadWriteFile :: IO ()
iomodeReadWriteFile = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    OSP.writeFile (baseDir </> [osp|foo|]) ""
    r <- try @IOException $ OSP.withFile (baseDir </> [osp|foo|]) ReadWriteMode $ \h -> do
      BS.hPut h "test"
      BS.hGetContents h
    Right "testtest"  @=? r


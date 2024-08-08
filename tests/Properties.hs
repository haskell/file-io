{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception
import qualified System.FilePath as FP
import Test.Tasty
import Test.Tasty.HUnit
import System.OsPath ((</>), osp, OsPath, OsString)
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
       , testCase "readFile . writeFile  . writeFile" writeWriteFileReadFile
       , testCase "readFile . appendFile . writeFile" appendFileReadFile
       , testCase "iomode: ReadFile does not allow write" iomodeReadFile
       , testCase "iomode: WriteFile does not allow read" iomodeWriteFile
       , testCase "iomode: AppendMode does not allow read" iomodeAppendFile
       , testCase "iomode: ReadWriteMode does allow everything" iomodeAppendFile
       , testCase "concurrency: open multiple handles (read and write)" concFile
       , testCase "concurrency: open multiple handles (read and read)" concFile2
       , testCase "concurrency: open multiple handles (write and write)" concFile3
       , testCase "openExistingFile no (Read)" existingFile
       , testCase "openExistingFile no (Write)" existingFile2
       , testCase "openExistingFile no (Append)" existingFile3
       , testCase "openExistingFile no (ReadWrite)" existingFile4
       , testCase "openExistingFile yes (Read)" existingFile'
       , testCase "openExistingFile yes (Write)" existingFile2'
       , testCase "openExistingFile yes (Append)" existingFile3'
       , testCase "openExistingFile yes (ReadWrite)" existingFile4'
       , testCase "openTempFile" (openTempFile2 OSP.openTempFile)
       , testCase "openTempFile (reopen file)" (openTempFile1 OSP.openTempFile)
       , testCase "openTempFile (filepaths different)" (openTempFile3 OSP.openTempFile)
       , testCase "openBinaryTempFile" (openTempFile2 OSP.openBinaryTempFile)
       , testCase "openBinaryTempFile (reopen file)" (openTempFile1 OSP.openBinaryTempFile)
       , testCase "openBinaryTempFile (filepaths different)" (openTempFile3 OSP.openBinaryTempFile)
       , testCase "openTempFileWithDefaultPermissions" (openTempFile2 OSP.openTempFileWithDefaultPermissions)
       , testCase "openTempFileWithDefaultPermissions (reopen file)" (openTempFile1 OSP.openTempFileWithDefaultPermissions)
       , testCase "openTempFileWithDefaultPermissions (filepaths different)" (openTempFile3 OSP.openTempFileWithDefaultPermissions)
       , testCase "openBinaryTempFileWithDefaultPermissions" (openTempFile2 OSP.openBinaryTempFileWithDefaultPermissions)
       , testCase "openBinaryTempFileWithDefaultPermissions (reopen file)" (openTempFile1 OSP.openBinaryTempFileWithDefaultPermissions)
       , testCase "openBinaryTempFileWithDefaultPermissions (filepaths different)" (openTempFile3 OSP.openBinaryTempFileWithDefaultPermissions)
       ]
    ]

writeFileReadFile :: Assertion
writeFileReadFile = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    OSP.writeFile (baseDir </> [osp|foo|]) "test"
    contents <- OSP.readFile (baseDir </> [osp|foo|])
    "test" @=? contents

writeWriteFileReadFile :: Assertion
writeWriteFileReadFile = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    OSP.writeFile (baseDir </> [osp|foo|]) "lol"
    OSP.writeFile (baseDir </> [osp|foo|]) "test"
    contents <- OSP.readFile (baseDir </> [osp|foo|])
    "test" @=? contents

appendFileReadFile :: Assertion
appendFileReadFile = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    OSP.writeFile (baseDir </> [osp|foo|]) "test"
    OSP.appendFile (baseDir </> [osp|foo|]) "test"
    contents <- OSP.readFile (baseDir </> [osp|foo|])
    "testtest" @=? contents

iomodeReadFile :: Assertion
iomodeReadFile = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    OSP.writeFile (baseDir </> [osp|foo|]) ""
    r <- try @IOException $ OSP.withFile (baseDir </> [osp|foo|]) ReadMode $ \h -> BS.hPut h "test"
    IOError Nothing IllegalOperation "hPutBuf" "handle is not open for writing" Nothing (Just $ baseDir' FP.</> "foo")
      @==? r

iomodeWriteFile :: Assertion
iomodeWriteFile = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    OSP.writeFile (baseDir </> [osp|foo|]) ""
    r <- try @IOException $ OSP.withFile (baseDir </> [osp|foo|]) WriteMode $ \h -> BS.hGetContents h
    IOError Nothing IllegalOperation "hGetBuf" "handle is not open for reading" Nothing (Just $ baseDir' FP.</> "foo")
      @==? r

iomodeAppendFile :: Assertion
iomodeAppendFile = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    OSP.writeFile (baseDir </> [osp|foo|]) ""
    r <- try @IOException $ OSP.withFile (baseDir </> [osp|foo|]) AppendMode $ \h -> BS.hGetContents h
    IOError Nothing IllegalOperation "hGetBuf" "handle is not open for reading" Nothing (Just $ baseDir' FP.</> "foo")
      @==? r

iomodeReadWriteFile :: Assertion
iomodeReadWriteFile = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    OSP.writeFile (baseDir </> [osp|foo|]) ""
    r <- try @IOException $ OSP.withFile (baseDir </> [osp|foo|]) ReadWriteMode $ \h -> do
      BS.hPut h "test"
      BS.hGetContents h
    Right "testtest"  @=? r

concFile :: Assertion
concFile = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    let fp = baseDir </> [osp|foo|]
    OSP.writeFile fp ""
    !h <- OSP.openFile fp ReadMode
    r <- try @IOException $ OSP.withFile fp WriteMode $ \h' -> do BS.hPut h' "test"
    _ <- try @IOException $ BS.hPut h ""
    IOError Nothing fileLockedType "withFile" fileLockedMsg Nothing (Just $ baseDir' FP.</> "foo") @==? r

concFile2 :: Assertion
concFile2 = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    let fp = baseDir </> [osp|foo|]
    OSP.writeFile fp "h"
    !h <- OSP.openFile fp ReadMode
    r <- try @IOException $ OSP.withFile fp ReadMode BS.hGetContents
    _ <- try @IOException $ BS.hPut h ""
    Right "h"  @=? r

concFile3 :: Assertion
concFile3 = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    let fp = baseDir </> [osp|foo|]
    OSP.writeFile fp ""
    !h <- OSP.openFile fp ReadMode
    r <- try @IOException $ OSP.withFile fp WriteMode (flip BS.hPut "test")
    _ <- try @IOException $ BS.hPut h ""
    IOError Nothing fileLockedType "withFile" fileLockedMsg Nothing (Just $ baseDir' FP.</> "foo") @==? r

existingFile :: Assertion
existingFile = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    let fp = baseDir </> [osp|foo|]
    r <- try @IOException $ OSP.openExistingFile fp ReadMode
    IOError Nothing NoSuchThing "openExistingFile" noSuchFileMsg Nothing (Just $ baseDir' FP.</> "foo") @==? r

existingFile2 :: Assertion
existingFile2 = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    let fp = baseDir </> [osp|foo|]
    r <- try @IOException $ OSP.openExistingFile fp WriteMode
    IOError Nothing NoSuchThing "openExistingFile" noSuchFileMsg Nothing (Just $ baseDir' FP.</> "foo") @==? r

existingFile3 :: Assertion
existingFile3 = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    let fp = baseDir </> [osp|foo|]
    r <- try @IOException $ OSP.openExistingFile fp AppendMode
    IOError Nothing NoSuchThing "openExistingFile" noSuchFileMsg Nothing (Just $ baseDir' FP.</> "foo") @==? r

existingFile4 :: Assertion
existingFile4 = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    let fp = baseDir </> [osp|foo|]
    r <- try @IOException $ OSP.openExistingFile fp AppendMode
    IOError Nothing NoSuchThing "openExistingFile" noSuchFileMsg Nothing (Just $ baseDir' FP.</> "foo") @==? r

existingFile' :: Assertion
existingFile' = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    let fp = baseDir </> [osp|foo|]
    OSP.writeFile fp "test"
    r <- try @IOException $ (OSP.openExistingFile fp ReadMode >>= BS.hGetContents)
    Right "test" @=? r

existingFile2' :: Assertion
existingFile2' = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    let fp = baseDir </> [osp|foo|]
    OSP.writeFile fp "test"
    r <- try @IOException $ do
      OSP.openExistingFile fp WriteMode >>= \h -> BS.hPut h "boo" >> hClose h
      OSP.readFile (baseDir </> [osp|foo|])
    Right "boo" @=? r

existingFile3' :: Assertion
existingFile3' = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    let fp = baseDir </> [osp|foo|]
    OSP.writeFile fp "test"
    r <- try @IOException $ do
      OSP.openExistingFile fp AppendMode >>= \h -> BS.hPut h "boo" >> hClose h
      OSP.readFile (baseDir </> [osp|foo|])
    Right "testboo" @=? r

existingFile4' :: Assertion
existingFile4' = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    let fp = baseDir </> [osp|foo|]
    OSP.writeFile fp "testx"
    r <- try @IOException $
      OSP.openExistingFile fp ReadWriteMode >>= \h -> do
        hSetBuffering h NoBuffering
        BS.hPut h "boo"
        !c <- BS.hGetSome h 5
        hSeek h AbsoluteSeek 0
        !c' <- BS.hGetSome h 5
        hClose h
        pure (c, c')
    Right ("tx", "bootx") @=? r

openTempFile1 :: (OsPath -> OsString -> IO (OsPath, Handle)) -> Assertion
openTempFile1 open = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    let file = [osp|foo.ext|]
    (!fp, h') <- open baseDir file
    hClose h'
    r <- try @IOException $ do
      OSP.openExistingFile fp ReadWriteMode >>= \h -> BS.hPut h "boo" >> hClose h
      OSP.readFile fp
    Right "boo" @=? r

openTempFile2 :: (OsPath -> OsString -> IO (OsPath, Handle)) -> Assertion
openTempFile2 open = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    let file = [osp|foo.ext|]
    (fp, h) <- open baseDir file
    r <- try @IOException $ do
      BS.hPut h "boo" >> hClose h
      OSP.readFile fp
    Right "boo" @=? r

openTempFile3 :: (OsPath -> OsString -> IO (OsPath, Handle)) -> Assertion
openTempFile3 open = do
  withSystemTempDirectory "test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    let file = [osp|foo.ext|]
    (!fp,  h) <- open baseDir file
    (!fp', h') <- open baseDir file
    hClose h
    hClose h'
    (fp /= fp') @? "Filepaths are different"


compareIOError :: forall a . (Eq a, Show a, HasCallStack) => IOException -> Either IOException a -> Assertion
compareIOError el (Left lel)  = lel { ioe_handle = Nothing
                                    , ioe_errno = Nothing
                                    } @?=
                                el  { ioe_handle = Nothing
                                    , ioe_errno = Nothing
                                    }
compareIOError el (Right rel) = Right rel @?= (Left el :: Either IOException a)

(@==?) :: forall a . (Eq a, Show a, HasCallStack) => IOException -> Either IOException a -> Assertion
(@==?) = compareIOError

noSuchFileMsg :: String
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
noSuchFileMsg = "The system cannot find the file specified."
#else
noSuchFileMsg = "No such file or directory"
#endif

fileLockedMsg :: String
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
fileLockedMsg = "The process cannot access the file because it is being used by another process."
#else
fileLockedMsg = "file is locked"
#endif

fileLockedType :: IOErrorType
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
fileLockedType = PermissionDenied
#else
fileLockedType = ResourceBusy
#endif


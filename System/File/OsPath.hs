{-# LANGUAGE TypeApplications #-}

module System.File.OsPath (
  openBinaryFile
, withFile
, withBinaryFile
, withFile'
, withBinaryFile'
, readFile
, readFile'
, writeFile
, writeFile'
, appendFile
, appendFile'
, openFile
, openExistingFile
) where

import qualified System.File.Platform as P

import Prelude ((.), ($), String, IO, pure, either, const, flip, Maybe(..), fmap, (<$>), id, ioError, (=<<), Bool(..))
import GHC.IO (catchException)
import GHC.IO.Exception (IOException(..))
import Control.DeepSeq (force)
import Control.Exception (SomeException, try, evaluate, bracket)
import System.IO (IOMode(..), Handle)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hSetBinaryMode, hClose)
import System.OsPath as OSP
import System.OsString.Internal.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- | Like 'openFile', but open the file in binary mode.
-- On Windows, reading a file in text mode (which is the default)
-- will translate CRLF to LF, and writing will translate LF to CRLF.
-- This is usually what you want with text files.  With binary files
-- this is undesirable; also, as usual under Microsoft operating systems,
-- text mode treats control-Z as EOF.  Binary mode turns off all special
-- treatment of end-of-line and end-of-file characters.
-- (See also 'System.IO.hSetBinaryMode'.)

-- On POSIX systems, 'openBinaryFile' is an /interruptible operation/ as
-- described in "Control.Exception".
openBinaryFile :: OsPath -> IOMode -> IO Handle
openBinaryFile osfp iomode = augmentError "openBinaryFile" osfp $ openBinaryFile' osfp iomode

openBinaryFile' :: OsPath -> IOMode -> IO Handle
openBinaryFile' (OsString fp) iomode =do
  h <- P.openFile fp iomode
  hSetBinaryMode h True
  pure h

-- | Run an action on a file.
--
-- The 'Handle' is automatically closed afther the action.
withFile :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withFile osfp@(OsString fp) iomode action = either ioError pure =<< (augmentError "withFile" osfp $ bracket
  (P.openFile fp iomode)
  hClose
  (try . action))

withBinaryFile :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile osfp iomode action = either ioError pure =<< (augmentError "withBinaryFile" osfp $ bracket
  (openBinaryFile' osfp iomode)
  hClose
  (try . action))

-- | Run an action on a file.
--
-- The 'Handle' is not automatically closed to allow lazy IO. Use this
-- with caution.
withFile'
  :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withFile' osfp@(OsString fp) iomode action = either ioError pure =<< (augmentError "withFile'" osfp $ do
  h <- P.openFile fp iomode
  try . action $ h)

withBinaryFile'
  :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile' fp iomode action = either ioError pure =<< (augmentError "withBinaryFile'" fp $ do
  h <- openBinaryFile' fp iomode
  try . action $ h)

-- | The 'readFile' function reads a file and returns the contents of the file
-- as a 'ByteString'. The file is read lazily, on demand.
readFile :: OsPath -> IO BSL.ByteString
readFile fp = withFile' fp ReadMode BSL.hGetContents

-- | The 'readFile'' function reads a file and returns the contents of the file
-- as a 'ByteString'. The file is fully read before being returned.
readFile'
  :: OsPath -> IO BS.ByteString
readFile' fp = withFile fp ReadMode BS.hGetContents

-- | The computation 'writeFile' @file str@ function writes the lazy 'ByteString' @str@,
-- to the file @file@.
writeFile :: OsPath -> BSL.ByteString -> IO ()
writeFile fp contents = withFile fp WriteMode (`BSL.hPut` contents)

-- | The computation 'writeFile' @file str@ function writes the strict 'ByteString' @str@,
-- to the file @file@.
writeFile'
  :: OsPath -> BS.ByteString -> IO ()
writeFile' fp contents = withFile fp WriteMode (`BS.hPut` contents)

-- | The computation 'appendFile' @file str@ function appends the lazy 'ByteString' @str@,
-- to the file @file@.
appendFile :: OsPath -> BSL.ByteString -> IO ()
appendFile fp contents = withFile fp AppendMode (`BSL.hPut` contents)

-- | The computation 'appendFile' @file str@ function appends the strict 'ByteString' @str@,
-- to the file @file@.
appendFile'
  :: OsPath -> BS.ByteString -> IO ()
appendFile' fp contents = withFile fp AppendMode (`BS.hPut` contents)

-- | Open a file and return the 'Handle'.
openFile :: OsPath -> IOMode -> IO Handle
openFile osfp@(OsString fp) = augmentError "openFile" osfp . P.openFile fp

-- | Open an existing file and return the 'Handle'.
openExistingFile :: OsPath -> IOMode -> IO Handle
openExistingFile osfp@(OsString fp) = augmentError "openExistingFile" osfp . P.openExistingFile fp

addFilePathToIOError :: String -> OsPath -> IOException -> IOException
addFilePathToIOError fun fp ioe = unsafePerformIO $ do
  fp'  <- either (const (fmap OSP.toChar . OSP.unpack $ fp)) id <$> try @SomeException (OSP.decodeFS fp)
  fp'' <- evaluate $ force fp'
  pure $ ioe{ ioe_location = fun, ioe_filename = Just fp'' }

augmentError :: String -> OsPath -> IO a -> IO a
augmentError str osfp = flip catchException (ioError . addFilePathToIOError str osfp)


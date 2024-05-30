module System.File.PlatformPath.Internal (
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
  , openFileWithCloseOnExec
  , openExistingFileWithCloseOnExec
  , OsPath.handleFinalizer
  , OsPath.HandleFinalizer
  , OsPath.addHandleFinalizer
  , withOpenFile'
  , addFilePathToIOError
  , augmentError
) where


import System.IO (IOMode(..), Handle)
import System.OsPath.Types
import GHC.IO.Exception (IOException(..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified System.File.OsPath.Internal as OsPath
import System.OsString.Internal.Types

import Data.Coerce (coerce)
import Prelude hiding (readFile, writeFile, appendFile)

-- | Like `OsPath.openBinaryFile`, but takes a `PlatformPath` instead of an `OsPath`.
openBinaryFile :: PlatformPath -> IOMode -> IO Handle
openBinaryFile = OsPath.openBinaryFile . coerce

-- | Like `OsPath.withFile`, but takes a `PlatformPath` instead of an `OsPath`.
withFile :: PlatformPath -> IOMode -> (Handle -> IO r) -> IO r
withFile = OsPath.withFile . coerce

-- | Like `OsPath.withBinaryFile`, but takes a `PlatformPath` instead of an `OsPath`.
withBinaryFile :: PlatformPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = OsPath.withBinaryFile . coerce

-- | Like `OsPath.withFile'`, but takes a `PlatformPath` instead of an `OsPath`.
withFile' :: PlatformPath -> IOMode -> (Handle -> IO r) -> IO r
withFile' = OsPath.withFile' . coerce

-- | Like `OsPath.withBinaryFile'`, but takes a `PlatformPath` instead of an `OsPath`.
withBinaryFile' :: PlatformPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile' = OsPath.withBinaryFile' . coerce

-- | Like `OsPath.readFile`, but takes a `PlatformPath` instead of an `OsPath`.
readFile :: PlatformPath -> IO BSL.ByteString
readFile = OsPath.readFile . coerce

-- | Like `OsPath.readFile'`, but takes a `PlatformPath` instead of an `OsPath`.
readFile' :: PlatformPath -> IO BS.ByteString
readFile' = OsPath.readFile' . coerce

-- | Like `OsPath.writeFile`, but takes a `PlatformPath` instead of an `OsPath`.
writeFile :: PlatformPath -> BSL.ByteString -> IO ()
writeFile = OsPath.writeFile . coerce

-- | Like `OsPath.writeFile'`, but takes a `PlatformPath` instead of an `OsPath`.
writeFile' :: PlatformPath -> BS.ByteString -> IO ()
writeFile' = OsPath.writeFile' . coerce

-- | Like `OsPath.appendFile`, but takes a `PlatformPath` instead of an `OsPath`.
appendFile :: PlatformPath -> BSL.ByteString -> IO ()
appendFile = OsPath.appendFile . coerce

-- | Like `OsPath.appendFile'`, but takes a `PlatformPath` instead of an `OsPath`.
appendFile' :: PlatformPath -> BS.ByteString -> IO ()
appendFile' = OsPath.appendFile' . coerce

-- | Like `OsPath.openFile`, but takes a `PlatformPath` instead of an `OsPath`.
openFile :: PlatformPath -> IOMode -> IO Handle
openFile = OsPath.openFile . coerce

-- | Like `OsPath.openExistingFile`, but takes a `PlatformPath` instead of an `OsPath`.
openExistingFile :: PlatformPath -> IOMode -> IO Handle
openExistingFile = OsPath.openExistingFile . coerce

-- | Open a file and return the 'Handle'.
--
-- Sets @O_CLOEXEC@ on posix.
--
-- @since 0.1.2
openFileWithCloseOnExec :: PlatformPath -> IOMode -> IO Handle
openFileWithCloseOnExec = OsPath.openFileWithCloseOnExec . coerce

-- | Open an existing file and return the 'Handle'.
--
-- Sets @O_CLOEXEC@ on posix.
--
-- @since 0.1.2
openExistingFileWithCloseOnExec :: PlatformPath -> IOMode -> IO Handle
openExistingFileWithCloseOnExec = OsPath.openExistingFileWithCloseOnExec . coerce

-- ---------------------------------------------------------------------------
-- Internals

withOpenFile' :: PlatformPath -> IOMode -> Bool -> Bool -> Bool -> (Handle -> IO r) -> Bool -> IO r
withOpenFile' = OsPath.withOpenFile' . coerce

addFilePathToIOError :: String -> PlatformPath -> IOException -> IOException
addFilePathToIOError = coerce OsPath.addFilePathToIOError

augmentError :: String -> PlatformPath -> IO a -> IO a
augmentError fp = OsPath.augmentError fp . coerce


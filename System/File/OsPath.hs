{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

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

import Prelude ((.), ($), String, IO, ioError, pure, either, const, flip, Maybe(..), fmap, (<$>), id, Bool(..), FilePath, (++), return, show, (>>=))
import GHC.IO (catchException)
import GHC.IO.Exception (IOException(..))
import GHC.IO.Handle (hClose_help)
import GHC.IO.Handle.Internals (debugIO)
import GHC.IO.Handle.Types (Handle__, Handle(..))
import Control.Concurrent.MVar
import Control.Monad (void, when)
import Control.DeepSeq (force)
import Control.Exception (SomeException, try, evaluate, mask, onException)
import System.IO (IOMode(..), hSetBinaryMode, hClose)
import System.IO.Unsafe (unsafePerformIO)
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
openBinaryFile osfp iomode = augmentError "openBinaryFile" osfp $ withOpenFile' osfp iomode True False pure False


-- | Run an action on a file.
--
-- The 'Handle' is automatically closed afther the action.
withFile :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withFile osfp iomode act = (augmentError "withFile" osfp
    $ withOpenFile' osfp iomode False False (try . act) True)
  >>= either ioError pure

withBinaryFile :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile osfp iomode act = (augmentError "withBinaryFile" osfp
    $ withOpenFile' osfp iomode True False (try . act) True)
  >>= either ioError pure

-- | Run an action on a file.
--
-- The 'Handle' is not automatically closed to allow lazy IO. Use this
-- with caution.
withFile'
  :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withFile' osfp iomode act = (augmentError "withFile'" osfp
    $ withOpenFile' osfp iomode False False (try . act) False)
  >>= either ioError pure

withBinaryFile'
  :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile' osfp iomode act = (augmentError "withBinaryFile'" osfp
    $ withOpenFile' osfp iomode True False (try . act) False)
  >>= either ioError pure

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
openFile osfp iomode = augmentError "openFile" osfp $ withOpenFile' osfp iomode False False pure False


-- | Open an existing file and return the 'Handle'.
openExistingFile :: OsPath -> IOMode -> IO Handle
openExistingFile osfp iomode = augmentError "openExistingFile" osfp $ withOpenFile' osfp iomode False True pure False


-- ---------------------------------------------------------------------------
-- Internals

handleFinalizer :: FilePath -> MVar Handle__ -> IO ()
handleFinalizer _fp m = do
  handle_ <- takeMVar m
  (handle_', _) <- hClose_help handle_
  putMVar m handle_'
  return ()

type HandleFinalizer = FilePath -> MVar Handle__ -> IO ()

-- | Add a finalizer to a 'Handle'. Specifically, the finalizer
-- will be added to the 'MVar' of a file handle or the write-side
-- 'MVar' of a duplex handle. See Handle Finalizers for details.
addHandleFinalizer :: Handle -> HandleFinalizer -> IO ()
addHandleFinalizer hndl finalizer = do
  debugIO $ "Registering finalizer: " ++ show filepath
  void $ mkWeakMVar mv (finalizer filepath mv)
  where
    !(filepath, !mv) = case hndl of
      FileHandle fp m -> (fp, m)
      DuplexHandle fp _ write_m -> (fp, write_m)

withOpenFile' :: OsPath -> IOMode -> Bool -> Bool -> (Handle -> IO r) -> Bool -> IO r
withOpenFile' (OsString fp) iomode binary existing action close_finally = mask $ \restore -> do
  hndl <- if existing
          then P.openExistingFile fp iomode
          else P.openFile fp iomode
  addHandleFinalizer hndl handleFinalizer
  when binary $ hSetBinaryMode hndl True
  r <- restore (action hndl) `onException` hClose hndl
  when close_finally $ hClose hndl
  pure r

addFilePathToIOError :: String -> OsPath -> IOException -> IOException
addFilePathToIOError fun fp ioe = unsafePerformIO $ do
  fp'  <- either (const (fmap OSP.toChar . OSP.unpack $ fp)) id <$> try @SomeException (OSP.decodeFS fp)
  fp'' <- evaluate $ force fp'
  pure $ ioe{ ioe_location = fun, ioe_filename = Just fp'' }

augmentError :: String -> OsPath -> IO a -> IO a
augmentError str osfp = flip catchException (ioError . addFilePathToIOError str osfp)


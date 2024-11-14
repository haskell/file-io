{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module System.File.OsPath.Internal where


import qualified System.File.Platform as P

import Prelude ((.), ($), String, IO, ioError, pure, either, const, flip, Maybe(..), fmap, (<$>), id, Bool(..), FilePath, (++), return, show, (>>=), (==), otherwise, userError)
import GHC.IO (catchException)
import GHC.IO.Exception (IOException(..))
import GHC.IO.Handle (hClose_help)
import GHC.IO.Handle.Internals (debugIO)
import GHC.IO.Handle.Types (Handle__, Handle(..))
import Control.Concurrent.MVar
import Control.Monad (void, when)
import Control.DeepSeq (force)
import Control.Exception (SomeException, try, evaluate, mask, onException, throwIO)
import System.IO (IOMode(..), hSetBinaryMode, hClose)
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath as OSP
import System.OsString.Internal.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import System.Posix.Types (CMode)
#if MIN_VERSION_filepath(1, 5, 0)
import qualified System.OsString as OSS
#else
import Data.Coerce
#endif

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
openBinaryFile osfp iomode = augmentError "openBinaryFile" osfp $ withOpenFile' osfp iomode True False False pure False


-- | Run an action on a file.
--
-- The 'Handle' is automatically closed afther the action.
withFile :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withFile osfp iomode act = (augmentError "withFile" osfp
    $ withOpenFile' osfp iomode False False False (try . act) True)
  >>= either ioError pure

withBinaryFile :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile osfp iomode act = (augmentError "withBinaryFile" osfp
    $ withOpenFile' osfp iomode True False False (try . act) True)
  >>= either ioError pure

-- | Run an action on a file.
--
-- The 'Handle' is not automatically closed to allow lazy IO. Use this
-- with caution.
withFile'
  :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withFile' osfp iomode act = (augmentError "withFile'" osfp
    $ withOpenFile' osfp iomode False False False (try . act) False)
  >>= either ioError pure

withBinaryFile'
  :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile' osfp iomode act = (augmentError "withBinaryFile'" osfp
    $ withOpenFile' osfp iomode True False False (try . act) False)
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
openFile osfp iomode = augmentError "openFile" osfp $ withOpenFile' osfp iomode False False False pure False


-- | Open an existing file and return the 'Handle'.
openExistingFile :: OsPath -> IOMode -> IO Handle
openExistingFile osfp iomode = augmentError "openExistingFile" osfp $ withOpenFile' osfp iomode False True False pure False

-- | Open a file and return the 'Handle'.
--
-- Sets @O_CLOEXEC@ on posix.
--
-- @since 0.1.2
openFileWithCloseOnExec :: OsPath -> IOMode -> IO Handle
openFileWithCloseOnExec osfp iomode = augmentError "openFileWithCloseOnExec" osfp $ withOpenFile' osfp iomode False False True pure False


-- | Open an existing file and return the 'Handle'.
--
-- Sets @O_CLOEXEC@ on posix.
--
-- @since 0.1.2
openExistingFileWithCloseOnExec :: OsPath -> IOMode -> IO Handle
openExistingFileWithCloseOnExec osfp iomode = augmentError "openExistingFileWithCloseOnExec" osfp $ withOpenFile' osfp iomode False True True pure False


-- | The function creates a temporary file in ReadWrite mode.
-- The created file isn\'t deleted automatically, so you need to delete it manually.
--
-- The file is created with permissions such that only the current
-- user can read\/write it.
--
-- With some exceptions (see below), the file will be created securely
-- in the sense that an attacker should not be able to cause
-- openTempFile to overwrite another file on the filesystem using your
-- credentials, by putting symbolic links (on Unix) in the place where
-- the temporary file is to be created.  On Unix the @O_CREAT@ and
-- @O_EXCL@ flags are used to prevent this attack, but note that
-- @O_EXCL@ is sometimes not supported on NFS filesystems, so if you
-- rely on this behaviour it is best to use local filesystems only.
--
-- @since 0.1.3
openTempFile :: OsPath     -- ^ Directory in which to create the file
             -> OsString   -- ^ File name template. If the template is \"foo.ext\" then
                           -- the created file will be \"fooXXX.ext\" where XXX is some
                           -- random number. Note that this should not contain any path
                           -- separator characters. On Windows, the template prefix may
                           -- be truncated to 3 chars, e.g. \"foobar.ext\" will be
                           -- \"fooXXX.ext\".
             -> IO (OsPath, Handle)
openTempFile tmp_dir template = openTempFile' "openTempFile" tmp_dir template False 0o600

-- | Like 'openTempFile', but opens the file in binary mode. See 'openBinaryFile' for more comments.
--
-- @since 0.1.3
openBinaryTempFile :: OsPath -> OsString -> IO (OsPath, Handle)
openBinaryTempFile tmp_dir template
    = openTempFile' "openBinaryTempFile" tmp_dir template True 0o600

-- | Like 'openTempFile', but uses the default file permissions
--
-- @since 0.1.3
openTempFileWithDefaultPermissions :: OsPath -> OsString
                                   -> IO (OsPath, Handle)
openTempFileWithDefaultPermissions tmp_dir template
    = openTempFile' "openTempFileWithDefaultPermissions" tmp_dir template False 0o666

-- | Like 'openBinaryTempFile', but uses the default file permissions
--
-- @since 0.1.3
openBinaryTempFileWithDefaultPermissions :: OsPath -> OsString
                                         -> IO (OsPath, Handle)
openBinaryTempFileWithDefaultPermissions tmp_dir template
    = openTempFile' "openBinaryTempFileWithDefaultPermissions" tmp_dir template True 0o666

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

withOpenFile' :: OsPath -> IOMode -> Bool -> Bool -> Bool -> (Handle -> IO r) -> Bool -> IO r
withOpenFile' (OsString fp) iomode binary existing cloExec action close_finally = mask $ \restore -> do
  hndl <- case (existing, cloExec) of
            (True, False) -> P.openExistingFile fp iomode
            (False, False) -> P.openFile fp iomode
            (True, True) -> P.openExistingFileWithCloseOnExec fp iomode
            (False, True) -> P.openFileWithCloseOnExec fp iomode
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


openTempFile' :: String -> OsPath -> OsString -> Bool -> CMode
              -> IO (OsPath, Handle)
openTempFile' loc (OsString tmp_dir) template@(OsString tmpl) binary mode
    | any_ (== OSP.pathSeparator) template
    = throwIO $ userError $ "openTempFile': Template string must not contain path separator characters: " ++ P.lenientDecode tmpl
    | otherwise = do
        (fp, hdl) <- P.findTempName (prefix, suffix) loc tmp_dir mode
        when binary $ hSetBinaryMode hdl True
        pure (OsString fp, hdl)
  where
    -- We split off the last extension, so we can use .foo.ext files
    -- for temporary files (hidden on Unix OSes). Unfortunately we're
    -- below filepath in the hierarchy here.
    (OsString prefix, OsString suffix) = OSP.splitExtension template

#if MIN_VERSION_filepath(1, 5, 0)
any_ :: (OsChar -> Bool) -> OsString -> Bool
any_ = OSS.any

#else
any_ :: (OsChar -> Bool) -> OsString -> Bool
any_ = coerce P.any_

#endif


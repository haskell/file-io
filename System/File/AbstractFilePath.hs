{-# LANGUAGE CPP #-}

module System.File.AbstractFilePath where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#define CTOR WS
import qualified System.File.AbstractFilePath.Windows as P
#else
#define CTOR PS
import qualified System.File.AbstractFilePath.Posix as P
#endif

import Control.Exception (bracket)
import System.IO (IOMode(..), Handle, hSetBinaryMode, hClose)
import System.AbstractFilePath
import System.OsString.Internal.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL


-- | Open a file and return the 'Handle'.
openFile :: AbstractFilePath -> IOMode -> IO Handle
openFile (OsString fp) = P.openFile fp

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
openBinaryFile :: AbstractFilePath -> IOMode -> IO Handle
openBinaryFile fp iomode = do
  h <- openFile fp iomode
  hSetBinaryMode h True
  pure h

-- | Run an action on a file.
--
-- The 'Handle' is automatically closed afther the action.
withFile :: AbstractFilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile fp iomode action = bracket
  (openFile fp iomode)
  hClose
  action

withBinaryFile :: AbstractFilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile fp iomode action = bracket
  (openBinaryFile fp iomode)
  hClose
  action

-- | Run an action on a file.
--
-- The 'Handle' is not automatically closed to allow lazy IO. Use this
-- with caution.
withFile' :: AbstractFilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile' fp iomode action = do
  h <- openFile fp iomode
  action h

withBinaryFile' :: AbstractFilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile' fp iomode action = do
  h <- openBinaryFile fp iomode
  action h

-- | The 'readFile' function reads a file and returns the contents of the file
-- as a 'ByteString'. The file is read lazily, on demand.
readFile :: AbstractFilePath -> IO BSL.ByteString
readFile fp = withFile' fp ReadMode BSL.hGetContents

-- | The 'readFile'' function reads a file and returns the contents of the file
-- as a 'ByteString'. The file is fully read before being returned.
readFile' :: AbstractFilePath -> IO BS.ByteString
readFile' fp = withFile fp ReadMode BS.hGetContents

-- | The computation 'writeFile' @file str@ function writes the lazy 'ByteString' @str@,
-- to the file @file@.
writeFile :: AbstractFilePath -> BSL.ByteString -> IO ()
writeFile fp contents = withFile fp WriteMode (`BSL.hPut` contents)

-- | The computation 'writeFile' @file str@ function writes the strict 'ByteString' @str@,
-- to the file @file@.
writeFile' :: AbstractFilePath -> BS.ByteString -> IO ()
writeFile' fp contents = withFile fp WriteMode (`BS.hPut` contents)

-- | The computation 'appendFile' @file str@ function appends the lazy 'ByteString' @str@,
-- to the file @file@.
appendFile :: AbstractFilePath -> BSL.ByteString -> IO ()
appendFile fp contents = withFile fp AppendMode (`BSL.hPut` contents)

-- | The computation 'appendFile' @file str@ function appends the strict 'ByteString' @str@,
-- to the file @file@.
appendFile' :: AbstractFilePath -> BS.ByteString -> IO ()
appendFile' fp contents = withFile fp AppendMode (`BS.hPut` contents)

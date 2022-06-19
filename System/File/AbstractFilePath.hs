{-# LANGUAGE CPP #-}

module System.File.AbstractFilePath where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#define CTOR WS
import qualified System.File.Windows as P
#else
#define CTOR PS
import qualified System.File.Posix as P
#endif

import Control.Exception (bracket)
import System.IO (IOMode(..), Handle, hSetBinaryMode, hClose)
import System.AbstractFilePath
import System.OsString.Internal.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

#define FILE_PATH AbstractFilePath
#include "Common.hs"

-- | Open a file and return the 'Handle'.
openFile :: AbstractFilePath -> IOMode -> IO Handle
openFile (OsString fp) = P.openFile fp

-- | Open an existing file and return the 'Handle'.
openExistingFile :: AbstractFilePath -> IOMode -> IO Handle
openExistingFile (OsString fp) = P.openExistingFile fp

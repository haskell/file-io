{-# LANGUAGE CPP #-}

module System.File.PlatformPath where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified System.File.Windows as P
#else
import qualified System.File.Posix as P
#endif

import Control.Exception (bracket)
import System.IO (IOMode(..), Handle, hSetBinaryMode, hClose)
import System.OsPath.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

#define FILE_PATH PlatformPath
#include "Common.hs"

-- | Open a file and return the 'Handle'.
openFile :: PlatformPath -> IOMode -> IO Handle
openFile fp = P.openFile fp

-- | Open an existing file and return the 'Handle'.
openExistingFile :: PlatformPath -> IOMode -> IO Handle
openExistingFile fp = P.openExistingFile fp


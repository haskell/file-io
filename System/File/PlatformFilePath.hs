{-# LANGUAGE CPP #-}

module System.File.PlatformFilePath where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified System.File.Windows as P
#else
import qualified System.File.Posix as P
#endif

import Control.Exception (bracket)
import System.IO (IOMode(..), Handle, hSetBinaryMode, hClose)
import System.AbstractFilePath.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

#define FILE_PATH PlatformFilePath
#include "Common.hs"

-- | Open a file and return the 'Handle'.
openFile :: PlatformFilePath -> IOMode -> IO Handle
openFile fp = P.openFile fp


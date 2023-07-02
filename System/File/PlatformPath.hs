{-# LANGUAGE CPP #-}

module System.File.PlatformPath where

import qualified System.File.Platform as P

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


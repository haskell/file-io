{-# LANGUAGE CPP #-}

module System.File.OsPath where

import qualified System.File.Platform as P

import Control.Exception (bracket)
import System.IO (IOMode(..), Handle, hSetBinaryMode, hClose)
import System.OsPath
import System.OsString.Internal.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

#define FILE_PATH OsPath
#include "Common.hs"

-- | Open a file and return the 'Handle'.
openFile :: OsPath -> IOMode -> IO Handle
openFile (OsString fp) = P.openFile fp

-- | Open an existing file and return the 'Handle'.
openExistingFile :: OsPath -> IOMode -> IO Handle
openExistingFile (OsString fp) = P.openExistingFile fp

{-# LANGUAGE TypeApplications #-}

module System.File.Platform where

import Control.Exception (try, onException, SomeException)
import GHC.IO.Handle.FD (fdToHandle')
import System.IO (IOMode(..), Handle)
import System.Posix.Types (Fd(..))
import System.Posix.IO.PosixString
    ( defaultFileFlags,
      openFd,
      closeFd,
      OpenFileFlags(noctty, nonBlock, creat, append, trunc, cloexec),
      OpenMode(ReadWrite, ReadOnly, WriteOnly) )
import System.OsPath.Posix ( PosixPath )
import qualified System.OsPath.Posix as PS

-- | Open a file and return the 'Handle'.
openFile :: PosixPath -> IOMode -> IO Handle
openFile = openFile_ defaultFileFlags'

openFile_ :: OpenFileFlags -> PosixPath -> IOMode -> IO Handle
openFile_ df fp iomode = fdToHandle_ iomode fp =<< case iomode of
  ReadMode      -> open ReadOnly  df
  WriteMode     -> open WriteOnly df { trunc = True, creat = Just 0o666 }
  AppendMode    -> open WriteOnly df { append = True, creat = Just 0o666 }
  ReadWriteMode -> open ReadWrite df { creat = Just 0o666 }
 where
  open = openFd fp

-- | Open an existing file and return the 'Handle'.
openExistingFile :: PosixPath -> IOMode -> IO Handle
openExistingFile = openExistingFile_ defaultExistingFileFlags

openExistingFile_ :: OpenFileFlags -> PosixPath -> IOMode -> IO Handle
openExistingFile_ df fp iomode = fdToHandle_ iomode fp =<< case iomode of
  ReadMode      -> open ReadOnly  df
  WriteMode     -> open WriteOnly df { trunc = True }
  AppendMode    -> open WriteOnly df { append = True }
  ReadWriteMode -> open ReadWrite df
 where
  open = openFd fp

fdToHandle_ :: IOMode -> PosixPath -> Fd -> IO Handle
fdToHandle_ iomode fp (Fd fd) = (`onException` closeFd (Fd fd)) $ do
    fp'  <- either (const (fmap PS.toChar . PS.unpack $ fp)) id <$> try @SomeException (PS.decodeFS fp)
    fdToHandle' fd Nothing False fp' iomode True

openFileWithCloseOnExec :: PosixPath -> IOMode -> IO Handle
openFileWithCloseOnExec = openFile_ defaultFileFlags' { cloexec = True }

openExistingFileWithCloseOnExec :: PosixPath -> IOMode -> IO Handle
openExistingFileWithCloseOnExec = openExistingFile_ defaultExistingFileFlags { cloexec = True }

defaultFileFlags' :: OpenFileFlags
defaultFileFlags' = defaultFileFlags { noctty = True, nonBlock = True }

defaultExistingFileFlags :: OpenFileFlags
defaultExistingFileFlags = defaultFileFlags { noctty = True, nonBlock = True, creat = Nothing }


module System.File.Posix where

import System.IO (IOMode(..), Handle)
import System.Posix.IO.PosixString
    ( defaultFileFlags,
      fdToHandle,
      openFd,
      OpenFileFlags(noctty, nonBlock, creat, append, trunc),
      OpenMode(ReadWrite, ReadOnly, WriteOnly) )
import System.OsPath.Posix ( PosixPath )

-- | Open a file and return the 'Handle'.
openFile :: PosixPath -> IOMode -> IO Handle
openFile fp iomode = fdToHandle =<< case iomode of
  ReadMode      -> open ReadOnly  df
  WriteMode     -> open WriteOnly df { trunc = True }
  AppendMode    -> open WriteOnly df { append = True }
  ReadWriteMode -> open ReadWrite df
 where
  open = openFd fp
  df = defaultFileFlags { noctty = True, nonBlock = True, creat = Just 0o666 }

-- | Open an existing file and return the 'Handle'.
openExistingFile :: PosixPath -> IOMode -> IO Handle
openExistingFile fp iomode = fdToHandle =<< case iomode of
  ReadMode      -> open ReadOnly  df
  WriteMode     -> open WriteOnly df { trunc = True }
  AppendMode    -> open WriteOnly df { append = True }
  ReadWriteMode -> open ReadWrite df
 where
  open = openFd fp
  df = defaultFileFlags { noctty = True, nonBlock = True, creat = Nothing }

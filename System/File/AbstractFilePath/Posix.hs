module System.File.AbstractFilePath.Posix where

import System.IO (IOMode(..), Handle)
import System.Posix.IO.PosixString
    ( defaultFileFlags,
      fdToHandle,
      openFd,
      OpenFileFlags(noctty, nonBlock, creat, append),
      OpenMode(ReadWrite, ReadOnly, WriteOnly) )
import System.AbstractFilePath.Posix ( PosixFilePath )

-- | Open a file and return the 'Handle'.
openFile :: PosixFilePath -> IOMode -> IO Handle
openFile fp iomode = fdToHandle =<< case iomode of
  ReadMode      -> open ReadOnly  df
  WriteMode     -> open WriteOnly df
  AppendMode    -> open WriteOnly df { append = True }
  ReadWriteMode -> open ReadWrite df
 where
  open = openFd fp
  df = defaultFileFlags { noctty = True, nonBlock = True, creat = Just 0o666 }


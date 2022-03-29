{-# LANGUAGE CPP #-}
module System.File.AbstractFilePath.Windows where

import Control.Exception (bracketOnError)
import Data.Bits
import System.IO (IOMode(..), Handle)
import System.AbstractFilePath.Windows ( WindowsFilePath )

import qualified System.Win32 as Win32
import qualified System.Win32.WindowsString.File as WS
#if defined(__IO_MANAGER_WINIO__)
import GHC.IO.SubSystem
#endif

-- | Open a file and return the 'Handle'.
openFile :: WindowsFilePath -> IOMode -> IO Handle
openFile fp iomode = bracketOnError
    (WS.createFile
      fp
      accessMode
      shareMode
      Nothing
      createMode
#if defined(__IO_MANAGER_WINIO__)
      (case ioSubSystem of
        IoPOSIX -> Win32.fILE_ATTRIBUTE_NORMAL
        IoNative -> Win32.fILE_ATTRIBUTE_NORMAL .|. Win32.fILE_FLAG_OVERLAPPED
      )
#else
      Win32.fILE_ATTRIBUTE_NORMAL
#endif
      Nothing)
    Win32.closeHandle
    Win32.hANDLEToHandle
 where
  accessMode = case iomode of
    ReadMode      -> Win32.gENERIC_READ
    WriteMode     -> Win32.gENERIC_WRITE
    AppendMode    -> Win32.fILE_APPEND_DATA
    ReadWriteMode -> Win32.gENERIC_READ .|. Win32.gENERIC_WRITE

  createMode = case iomode of
    ReadMode      -> Win32.oPEN_ALWAYS
    WriteMode     -> Win32.cREATE_ALWAYS
    AppendMode    -> Win32.oPEN_ALWAYS
    ReadWriteMode -> Win32.cREATE_ALWAYS

  shareMode = case iomode of
    ReadMode      -> maxShareMode
    WriteMode     -> writeShareMode
    AppendMode    -> writeShareMode
    ReadWriteMode -> maxShareMode

maxShareMode :: Win32.ShareMode
maxShareMode =
  Win32.fILE_SHARE_DELETE .|.
  Win32.fILE_SHARE_READ   .|.
  Win32.fILE_SHARE_WRITE

writeShareMode :: Win32.ShareMode
writeShareMode =
  Win32.fILE_SHARE_DELETE .|.
  Win32.fILE_SHARE_READ

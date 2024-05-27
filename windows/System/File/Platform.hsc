{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module System.File.Platform where

import Control.Exception (bracketOnError, try, SomeException, onException)
import Data.Bits
import System.IO (IOMode(..), Handle)
import System.OsPath.Windows ( WindowsPath )
import qualified System.OsPath.Windows as WS
import Foreign.C.Types
import Foreign.Ptr (ptrToIntPtr)

import qualified System.Win32 as Win32
import qualified System.Win32.WindowsString.File as WS
import Control.Monad (when, void)
#if defined(__IO_MANAGER_WINIO__)
import GHC.IO.SubSystem
#else
import GHC.IO.Handle.FD (fdToHandle')
#include <fcntl.h>
#endif

-- | Open a file and return the 'Handle'.
openFile :: WindowsPath -> IOMode -> IO Handle
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
    toHandle
 where
#if defined(__IO_MANAGER_WINIO__)
  toHandle h = (`onException` Win32.closeHandle h) $ do
    when (iomode == AppendMode ) $ void $ Win32.setFilePointerEx h 0 Win32.fILE_END
    Win32.hANDLEToHandle h
#else
  toHandle h = (`onException` Win32.closeHandle h) $ do
    when (iomode == AppendMode ) $ void $ Win32.setFilePointerEx h 0 Win32.fILE_END
    fd <- _open_osfhandle (fromIntegral (ptrToIntPtr h)) (#const _O_BINARY)
    fp' <- either (const (fmap WS.toChar . WS.unpack $ fp)) id <$> try @SomeException (WS.decodeFS fp)
    fdToHandle' fd Nothing False fp' iomode True
#endif
  accessMode = case iomode of
    ReadMode      -> Win32.gENERIC_READ
    WriteMode     -> Win32.gENERIC_WRITE
    AppendMode    -> Win32.gENERIC_WRITE .|. Win32.fILE_APPEND_DATA
    ReadWriteMode -> Win32.gENERIC_READ .|. Win32.gENERIC_WRITE

  createMode = case iomode of
    ReadMode      -> Win32.oPEN_EXISTING
    WriteMode     -> Win32.cREATE_ALWAYS
    AppendMode    -> Win32.oPEN_ALWAYS
    ReadWriteMode -> Win32.oPEN_ALWAYS

  shareMode = case iomode of
    ReadMode      -> Win32.fILE_SHARE_READ
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

  -- | Open an existing file and return the 'Handle'.
openExistingFile :: WindowsPath -> IOMode -> IO Handle
openExistingFile fp iomode = bracketOnError
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
    toHandle
 where
  toHandle h = do
    when (iomode == AppendMode ) $ void $ Win32.setFilePointerEx h 0 Win32.fILE_END
    Win32.hANDLEToHandle h
  accessMode = case iomode of
    ReadMode      -> Win32.gENERIC_READ
    WriteMode     -> Win32.gENERIC_WRITE
    AppendMode    -> Win32.gENERIC_WRITE .|. Win32.fILE_APPEND_DATA
    ReadWriteMode -> Win32.gENERIC_READ .|. Win32.gENERIC_WRITE

  createMode = case iomode of
    ReadMode      -> Win32.oPEN_EXISTING
    WriteMode     -> Win32.tRUNCATE_EXISTING
    AppendMode    -> Win32.oPEN_EXISTING
    ReadWriteMode -> Win32.oPEN_EXISTING

  shareMode = case iomode of
    ReadMode      -> Win32.fILE_SHARE_READ
    WriteMode     -> writeShareMode
    AppendMode    -> writeShareMode
    ReadWriteMode -> maxShareMode

#if !defined(__IO_MANAGER_WINIO__)
foreign import ccall "_open_osfhandle"
  _open_osfhandle :: CIntPtr -> CInt -> IO CInt
#endif

openFileWithCloseOnExec :: WindowsPath -> IOMode -> IO Handle
openFileWithCloseOnExec = openFile

openExistingFileWithCloseOnExec :: WindowsPath -> IOMode -> IO Handle
openExistingFileWithCloseOnExec = openExistingFile


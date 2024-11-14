{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PackageImports   #-}

module System.File.Platform where

import Control.Exception (bracketOnError, try, SomeException, onException)
import Data.Bits
import Data.Maybe (fromJust)
import System.IO (IOMode(..), Handle)
import System.OsPath.Windows ( WindowsPath )
import qualified System.OsPath.Windows as WS
import Foreign.C.Types

import qualified System.OsString.Windows as WS hiding (decodeFS)
import System.OsString.Windows ( encodeUtf, WindowsString )
import qualified System.Win32 as Win32
import qualified System.Win32.WindowsString.File as WS
import System.Win32.WindowsString.Types (withTString, peekTString)
#if MIN_VERSION_Win32(2, 14, 0)
import System.Win32.WindowsString.Types (withFilePath)
#endif
import Control.Monad (when, void)
#if defined(__IO_MANAGER_WINIO__)
import GHC.IO.SubSystem
#else
import GHC.IO.Handle.FD (fdToHandle')
#include <fcntl.h>
#endif
import GHC.IORef (atomicModifyIORef'_)
import Foreign.C (getErrno, errnoToIOError)
import Data.IORef (IORef, newIORef)
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils (with)
import Foreign.Storable
import System.CPUTime (cpuTimePrecision, getCPUTime)
import System.Posix.Types (CMode)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Internals (c_getpid, o_EXCL)
import Text.Printf (printf)

#if MIN_VERSION_filepath(1, 5, 0)
import System.OsString.Encoding
import "os-string" System.OsString.Internal.Types (WindowsString(..), WindowsChar(..))
import qualified "os-string" System.OsString.Data.ByteString.Short as BC
#else
import Data.Coerce (coerce)
import System.OsPath.Encoding
import "filepath" System.OsString.Internal.Types (WindowsString(..), WindowsChar(..))
import qualified "filepath" System.OsPath.Data.ByteString.Short.Word16 as BC
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
    (toHandle fp iomode)
 where
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
    (toHandle fp iomode)
 where
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

findTempName :: (WindowsString, WindowsString)
             -> String
             -> WindowsPath
             -> CMode
             -> IO (WindowsPath, Handle)
findTempName (prefix, suffix) loc tmp_dir mode = go
 where
  go = do
    let label = if prefix == mempty then fromJust (encodeUtf "ghc") else prefix
#if MIN_VERSION_Win32(2, 14, 0)
    withFilePath tmp_dir $ \c_tmp_dir ->
#else
    withTString tmp_dir $ \c_tmp_dir ->
#endif
      withTString label $ \c_template ->
        withTString suffix $ \c_suffix ->
#if MIN_VERSION_base(4, 15, 0)
          with nullPtr $ \c_ptr -> do
            res <- c_createUUIDTempFileErrNo c_tmp_dir c_template c_suffix c_ptr
            if not res
               then do errno <- getErrno
                       ioError (errnoToIOError loc errno Nothing (Just $ lenientDecode tmp_dir))
               else do c_p <- peek c_ptr
                       filename <- peekTString c_p
                       free c_p
                       let flags = fromIntegral mode .&. o_EXCL
                       handleResultsWinIO filename (flags == o_EXCL)
#else
            -- NOTE: revisit this when new I/O manager in place and use a UUID
            --       based one when we are no longer MAX_PATH bound.
            allocaBytes (sizeOf (undefined :: CWchar) * 260) $ \c_str -> do
            res <- c_getTempFileNameErrorNo c_tmp_dir c_template c_suffix 0
                                            c_str
            if not res
               then do errno <- getErrno
                       ioError (errnoToIOError loc errno Nothing (Just $ lenientDecode tmp_dir))
               else do filename <- peekTString c_str
                       let flags = fromIntegral mode .&. o_EXCL
                       handleResultsWinIO filename (flags == o_EXCL)
#endif

  handleResultsWinIO filename excl = do
    h <- (if excl then openExistingFile else openFile) filename ReadWriteMode
    return (filename, h)

#if MIN_VERSION_base(4, 15, 0)
foreign import ccall "__createUUIDTempFileErrNo" c_createUUIDTempFileErrNo
  :: CWString -> CWString -> CWString -> Ptr CWString -> IO Bool
#else
foreign import ccall "getTempFileNameErrorNo" c_getTempFileNameErrorNo
  :: CWString -> CWString -> CWString -> CUInt -> Ptr CWchar -> IO Bool
#endif



tempCounter :: IORef Int
tempCounter = unsafePerformIO $ newIORef 0
{-# NOINLINE tempCounter #-}

-- build large digit-alike number
rand_string :: IO WindowsPath
rand_string = do
  r1 <- fromIntegral @_ @Int <$> c_getpid
  (r2, _) <- atomicModifyIORef'_ tempCounter (+1)
  r3 <- (`quot` cpuTimePrecision) <$> getCPUTime
  return $ WS.pack $ fmap (WS.unsafeFromChar) (printf "%x-%x-%x" r1 r2 r3)

lenientDecode :: WindowsString -> String
lenientDecode ws = let utf16le' = WS.decodeWith utf16le_b ws
                       ucs2' = WS.decodeWith ucs2le ws
                   in case (utf16le', ucs2') of
                        (Right s, ~_) -> s
                        (_, Right s) -> s
                        (Left _, Left _) -> error "lenientDecode: failed to decode"


toHandle :: WindowsPath -> IOMode -> Win32.HANDLE -> IO Handle
#if defined(__IO_MANAGER_WINIO__)
toHandle _ iomode h = (`onException` Win32.closeHandle h) $ do
    when (iomode == AppendMode ) $ void $ Win32.setFilePointerEx h 0 Win32.fILE_END
    Win32.hANDLEToHandle h
#else
toHandle fp iomode h = (`onException` Win32.closeHandle h) $ do
    when (iomode == AppendMode ) $ void $ Win32.setFilePointerEx h 0 Win32.fILE_END
    fd <- _open_osfhandle (fromIntegral (ptrToIntPtr h)) (#const _O_BINARY)
    fp' <- either (const (fmap WS.toChar . WS.unpack $ fp)) id <$> try @SomeException (WS.decodeFS fp)
    fdToHandle' fd Nothing False fp' iomode True
#endif

#if !MIN_VERSION_filepath(1, 5, 0)

any_ :: (WindowsChar -> Bool) -> WindowsString -> Bool
any_ = coerce BC.any

#endif


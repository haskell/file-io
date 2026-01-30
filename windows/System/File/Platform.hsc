{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PackageImports   #-}
{-# LANGUAGE MultiWayIf   #-}

module System.File.Platform where

import Control.Exception (bracketOnError, try, SomeException, onException)
import Data.Bits
import Data.Coerce
import Data.Maybe (fromJust)
import System.IO (IOMode(..), Handle, hSetEncoding)
import System.OsPath.Windows ( WindowsPath )
import qualified System.OsPath.Windows as OSP
import qualified System.OsPath.Windows as WS
import Foreign.C.Types

import System.OsString.Windows ( encodeUtf, WindowsString, WindowsChar )
import qualified System.Win32 as Win32
import qualified System.Win32.WindowsString.File as WS
import System.Win32.WindowsString.Types (withTString, peekTString)
import GHC.IO.Encoding (getLocaleEncoding)
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
import qualified "os-string" System.OsString.Data.ByteString.Short.Word16 as BC
import "os-string" System.OsString.Internal.Types (WindowsString(..), WindowsChar(..))
#else
import Data.Coerce (coerce)
import System.OsPath.Encoding
import "filepath" System.OsString.Internal.Types (WindowsString(..), WindowsChar(..))
import qualified "filepath" System.OsPath.Data.ByteString.Short.Word16 as BC
#endif

import System.IO.Error (modifyIOError, ioeSetFileName)
import GHC.IO.Encoding.UTF16 (mkUTF16le)
import GHC.IO.Encoding.Failure (CodingFailureMode(TransliterateCodingFailure))
import Control.Exception (displayException, Exception)

#if defined(LONG_PATHS)
import System.IO.Error (ioeSetLocation, ioeGetLocation, catchIOError)
import Data.Char (isAlpha, isAscii, toUpper)
import qualified System.Win32.WindowsString.Info as WS
#endif

-- | Open a file and return the 'Handle'.
openFile :: WindowsPath -> IOMode -> IO Handle
openFile fp' iomode = (`ioeSetWsPath` fp') `modifyIOError` do
  fp <- furnishPath fp'
  bracketOnError
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
    (toHandle fp' iomode)
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
openExistingFile fp' iomode = (`ioeSetWsPath` fp') `modifyIOError` do
  fp <- furnishPath fp'
  bracketOnError
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
             -> Bool
             -> IO (WindowsPath, Handle)
findTempName (prefix, suffix) loc tmp_dir mode _cloExec = go
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
lenientDecode wstr = let utf16le' = WS.decodeWith utf16le_b wstr
                         ucs2' = WS.decodeWith ucs2le wstr
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
    h' <- fdToHandle' fd Nothing False fp' iomode True
    getLocaleEncoding >>= hSetEncoding h'
    pure h'
#endif

#if !MIN_VERSION_filepath(1, 5, 0)

any_ :: (WindowsChar -> Bool) -> WindowsString -> Bool
any_ = coerce BC.any

#endif

ioeSetWsPath :: IOError -> WindowsPath -> IOError
ioeSetWsPath err =
  ioeSetFileName err .
  rightOrError .
  WS.decodeWith (mkUTF16le TransliterateCodingFailure)

rightOrError :: Exception e => Either e a -> a
rightOrError (Left e)  = error (displayException e)
rightOrError (Right a) = a

-- inlined stuff from directory package
furnishPath :: WindowsPath -> IO WindowsPath
#if !defined(LONG_PATHS)
furnishPath path = pure path
#else
furnishPath path =
  (toExtendedLengthPath <$> rawPrependCurrentDirectory path)
    `catchIOError` \ _ ->
      pure path

toExtendedLengthPath :: WindowsPath -> WindowsPath
toExtendedLengthPath path =
  if WS.isRelative path
  then simplifiedPath
  else
    if | ws "\\??\\"  `isPrefixOf'` simplifiedPath -> simplifiedPath
       | ws "\\\\?\\" `isPrefixOf'` simplifiedPath -> simplifiedPath
       | ws "\\\\.\\" `isPrefixOf'` simplifiedPath -> simplifiedPath
       | ws "\\\\"    `isPrefixOf'` simplifiedPath -> ws "\\\\?\\UNC" <> drop' 1 simplifiedPath
       | otherwise                                    -> ws "\\\\?\\" <> simplifiedPath
  where simplifiedPath = simplifyWindows path

rawPrependCurrentDirectory :: WindowsPath -> IO WindowsPath
rawPrependCurrentDirectory path
  | WS.isRelative path =
    ((`ioeAddLocation` "prependCurrentDirectory") .
     (`ioeSetWsPath` path)) `modifyIOError` do
      getFullPathName path
  | otherwise = pure path

simplifyWindows :: WindowsPath -> WindowsPath
simplifyWindows path
  | path == mempty         = mempty
  | drive' == ws "\\\\?\\" = drive' <> subpath
  | otherwise              = simplifiedPath
  where
    simplifiedPath = WS.joinDrive drive' subpath'
    (drive, subpath) = WS.splitDrive path
    drive' = upperDrive (normaliseTrailingSep (normalisePathSeps drive))
    subpath' = appendSep . avoidEmpty . prependSep . WS.joinPath .
               stripPardirs . expandDots . skipSeps .
               WS.splitDirectories $ subpath

    upperDrive d = case WS.unpack d of
      c : k : s
        | isAlpha (WS.toChar c), WS.toChar k == ':', all WS.isPathSeparator s ->
          -- unsafeFromChar is safe here since all characters are ASCII.
          WS.pack (WS.unsafeFromChar (toUpper (WS.toChar c)) : WS.unsafeFromChar ':' : s)
      _ -> d
    skipSeps =
      (WS.pack <$>) .
      filter (not . (`elem` (pure <$> WS.pathSeparators))) .
      (WS.unpack <$>)
    stripPardirs | pathIsAbsolute || subpathIsAbsolute = dropWhile (== ws "..")
                 | otherwise = id
    prependSep | subpathIsAbsolute = (WS.pack [WS.pathSeparator] <>)
               | otherwise = id
    avoidEmpty | not pathIsAbsolute
               , drive == mempty || hasTrailingPathSep -- prefer "C:" over "C:."
                 = emptyToCurDir
               | otherwise = id
    appendSep p | hasTrailingPathSep, not (pathIsAbsolute && p == mempty)
                  = WS.addTrailingPathSeparator p
                | otherwise = p
    pathIsAbsolute = not (WS.isRelative path)
    subpathIsAbsolute = any WS.isPathSeparator (take 1 (WS.unpack subpath))
    hasTrailingPathSep = WS.hasTrailingPathSeparator subpath

expandDots :: [WindowsPath] -> [WindowsPath]
expandDots = reverse . go []
  where
    go ys' xs' =
      case xs' of
        [] -> ys'
        x : xs
          | x == ws "." -> go ys' xs
          | x == ws ".." ->
              case ys' of
                [] -> go (x : ys') xs
                y : ys
                  | y == ws ".." -> go (x : ys') xs
                  | otherwise -> go ys xs
          | otherwise -> go (x : ys') xs

-- | Remove redundant trailing slashes and pick the right kind of slash.
normaliseTrailingSep :: WindowsPath -> WindowsPath
normaliseTrailingSep path = do
  let path' = reverse (WS.unpack path)
  let (sep, path'') = span WS.isPathSeparator path'
  let addSep = if null sep then id else (WS.pathSeparator :)
  WS.pack (reverse (addSep path''))

normalisePathSeps :: WindowsPath -> WindowsPath
normalisePathSeps p = WS.pack (normaliseChar <$> WS.unpack p)
  where normaliseChar c = if WS.isPathSeparator c then WS.pathSeparator else c

emptyToCurDir :: WindowsPath -> WindowsPath
emptyToCurDir path
  | path == mempty = ws "."
  | otherwise      = path

ws :: String -> WindowsString
ws = rightOrError . WS.encodeUtf

getFullPathName :: WindowsPath -> IO WindowsPath
getFullPathName path =
  fromExtendedLengthPath <$> WS.getFullPathName (toExtendedLengthPath path)

ioeAddLocation :: IOError -> String -> IOError
ioeAddLocation e loc = do
  ioeSetLocation e newLoc
  where
    newLoc = loc <> if null oldLoc then "" else ":" <> oldLoc
    oldLoc = ioeGetLocation e

fromExtendedLengthPath :: WindowsPath -> WindowsPath
fromExtendedLengthPath ePath
  | ws "\\\\?\\" `isPrefixOf'` ePath
  , let eSubpath = drop' 4 ePath
  = if | ws "UNC\\" `isPrefixOf'` eSubpath
       -> ws "\\\\" <> drop' 4 eSubpath
       | Just (drive, rest)    <- uncons' eSubpath
       , Just (col,   subpath) <- uncons' rest
       , WS.toChar col == ':'
       , isDriveChar drive
       , isPathRegular subpath
       -> eSubpath
       | otherwise
       -> ePath
  | otherwise = ePath
  where
    isDriveChar drive = isAlpha (WS.toChar drive) && isAscii (WS.toChar drive)
    isPathRegular path =
      not (WS.unsafeFromChar '/' `elem'` path ||
           any (\x -> x == ws ".." || x == ws ".") (WS.splitDirectories path))

elem' :: WindowsChar -> WindowsString -> Bool
elem' = coerce BC.elem

isPrefixOf' :: WindowsString -> WindowsString -> Bool
isPrefixOf' = coerce BC.isPrefixOf

drop' :: Int -> WindowsString -> WindowsString
drop' = coerce BC.drop

uncons' :: WindowsString -> Maybe (WindowsChar, WindowsString)
uncons' = coerce BC.uncons

#endif

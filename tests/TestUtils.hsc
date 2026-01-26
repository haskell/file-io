{-# LANGUAGE CPP #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}

module TestUtils where

#if !defined(mingw32_HOST_OS)
#include <fcntl.h>
#include <sys/stat.h>
#endif

import Control.Monad (when)
import Control.Exception (bracket, displayException, Exception)
import Data.Maybe
import Data.Foldable (sequenceA_)
import System.IO.Error
import GHC.IO.Exception
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Data.Bits

import System.OsPath hiding (OsString)
#if MIN_VERSION_filepath(1, 5, 0)
import "os-string" System.OsString.Internal.Types (OsString(OsString), getOsString)
#else
import "filepath" System.OsString.Internal.Types (OsString(OsString), getOsString)
#endif
import System.File.OsPath

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified System.Win32.WindowsString.File as Win32
import qualified System.Win32.WindowsString.Info as Win32
import qualified System.Win32.WindowsString.Shell as Win32
import qualified System.Win32.WindowsString.Time as Win32
import qualified System.Win32.WindowsString.Types as Win32
import qualified System.Win32.WindowsString.Console as Win32
#else
import qualified System.Posix.Env.PosixString as Posix
import qualified System.Posix.PosixPath.FilePath as Posix
import qualified System.Posix.Files as Posix (FileStatus(..))
import qualified System.Posix.Files.PosixString as Posix
import qualified System.Posix.Types as Posix
import qualified System.Posix.Internals as Posix (CStat)
import qualified System.Posix.Directory.PosixPath as Posix
import qualified System.Posix.IO.PosixString as Posix
import qualified System.Posix.Directory.Fd as Posix
#endif




withSystemTempDirectory :: OsPath -> (OsPath -> IO a) -> IO a
withSystemTempDirectory template action = do
  tmp <- getTemporaryDirectory
  bracket
    (createTempDirectory tmp template)
    (removeDirectoryRecursive)
    action

withFileIOTestDir :: (OsPath -> IO a) -> IO a
withFileIOTestDir action = withSystemTempDirectory [osp|file-io-test|] action


-- inlined from directory
--

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
type RawHandle = OsPath
type Metadata = Win32.BY_HANDLE_FILE_INFORMATION
type Mode = Win32.FileAttributeOrFlag

getTemporaryInternal :: IO OsPath
getTemporaryInternal = OsString <$> Win32.getTemporaryDirectory

fileTypeFromMetadata :: Metadata -> FileType
fileTypeFromMetadata info
  | isLink    = if isDir then DirectoryLink else SymbolicLink
  | isDir     = Directory
  | otherwise = File
  where
    isLink = attrs .&. Win32.fILE_ATTRIBUTE_REPARSE_POINT /= 0
    isDir  = attrs .&. Win32.fILE_ATTRIBUTE_DIRECTORY /= 0
    attrs  = Win32.bhfiFileAttributes info

filesAlwaysRemovable :: Bool
filesAlwaysRemovable = False

setModeAt :: Maybe RawHandle -> OsPath -> Mode -> IO ()
setModeAt dir path = setFileMode (pathAt dir path)

setForceRemoveMode :: Mode -> Mode
setForceRemoveMode m = m .&. complement Win32.fILE_ATTRIBUTE_READONLY

modeFromMetadata :: Metadata -> Mode
modeFromMetadata = Win32.bhfiFileAttributes

getMetadataAt :: WhetherFollow -> Maybe RawHandle -> OsPath -> IO Metadata
getMetadataAt NoFollow    dir path = getSymbolicLinkMetadata (pathAt dir path)
getMetadataAt FollowLinks dir path = getFileMetadata         (pathAt dir path)

readDirToEnd :: RawHandle -> IO [OsPath]
readDirToEnd = getDirectoryContentsInternal

openRaw :: WhetherFollow -> Maybe RawHandle -> OsPath -> IO RawHandle
openRaw _ dir path = pure (pathAt dir path)

closeRaw :: RawHandle -> IO ()
closeRaw _ = pure ()

getDirectoryContentsInternal :: OsPath -> IO [OsPath]
getDirectoryContentsInternal path = do
  query <- furnishPath (path </> os "*")
  bracket
    (Win32.findFirstFile query)
    (\ (h, _) -> Win32.findClose h)
    (\ (h, fdat) -> loop h fdat [])
  where
    -- we needn't worry about empty directories: a directory always
    -- has at least "." and ".." entries
    loop :: Win32.HANDLE -> Win32.FindData -> [OsPath] -> IO [OsPath]
    loop h fdat acc = do
      filename <- Win32.getFindDataFileName fdat
      more <- Win32.findNextFile h fdat
      if more
        then loop h fdat (OsString filename : acc)
        else pure (OsString filename : acc)
             -- no need to reverse, ordering is undefined
             --
removePathAt :: FileType -> Maybe RawHandle -> OsPath -> IO ()
removePathAt ty dir path = removePathInternal isDir (pathAt dir path)
  where isDir = fileTypeIsDirectory ty

removePathInternal :: Bool -> OsPath -> IO ()
removePathInternal isDir path =
  (`ioeSetOsPath` path) `modifyIOError` do
    furnishPath path
      >>= if isDir then Win32.removeDirectory else Win32.deleteFile

furnishPath :: OsPath -> IO WindowsPath
furnishPath path =
  (toExtendedLengthPath <$> rawPrependCurrentDirectory path)
    `catchIOError` \ _ ->
      pure (getOsString path)

toExtendedLengthPath :: OsPath -> WindowsPath
toExtendedLengthPath path =
  getOsString $
  if isRelative path
  then simplifiedPath
  else
    case toChar <$> simplifiedPath' of
      '\\' : '?'  : '?' : '\\' : _ -> simplifiedPath
      '\\' : '\\' : _ -> simplifiedPath
      _ -> os "\\\\?\\" <> simplifiedPath
  where simplifiedPath = simplify path
        simplifiedPath' = unpack simplifiedPath

rawPrependCurrentDirectory :: OsPath -> IO OsPath
rawPrependCurrentDirectory path
  | isRelative path =
    ((`ioeAddLocation` "prependCurrentDirectory") .
     (`ioeSetOsPath` path)) `modifyIOError` do
      getFullPathName path
  | otherwise = pure path

getFullPathName :: OsPath -> IO OsPath
getFullPathName path =
  fromExtendedLengthPath <$> Win32.getFullPathName (toExtendedLengthPath path)

fromExtendedLengthPath :: WindowsPath -> OsPath
fromExtendedLengthPath ePath' =
  case unpack ePath of
    c1 : c2 : c3 : c4 : path
      | (toChar <$> [c1, c2, c3, c4]) == "\\\\?\\" ->
      case path of
        c5 : c6 : c7 : subpath@(c8 : _)
          | (toChar <$> [c5, c6, c7, c8]) == "UNC\\" ->
            pack (c8 : subpath)
        drive : col : subpath
          -- if the path is not "regular", then the prefix is necessary
          -- to ensure the path is interpreted literally
          | toChar col == ':', isDriveChar drive, isPathRegular subpath ->
            pack path
        _ -> ePath
    _ -> ePath
  where
    ePath = OsString ePath'
    isDriveChar drive = isAlpha (toChar drive) && isAscii (toChar drive)
    isPathRegular path =
      not ('/' `elem` (toChar <$> path) ||
           os "." `elem` splitDirectories (pack path) ||
           os ".." `elem` splitDirectories (pack path))

getSymbolicLinkMetadata :: OsPath -> IO Metadata
getSymbolicLinkMetadata path =
  (`ioeSetOsPath` path) `modifyIOError` do
    path' <- furnishPath path
    let open = Win32.createFile path' 0 maxShareMode Nothing Win32.oPEN_EXISTING
                                (Win32.fILE_FLAG_BACKUP_SEMANTICS .|.
                                 win32_fILE_FLAG_OPEN_REPARSE_POINT) Nothing
    bracket open Win32.closeHandle $ \ h -> do
      Win32.getFileInformationByHandle h
#else

type RawHandle = Posix.Fd
type Metadata = Posix.FileStatus
type Mode = Posix.FileMode

getTemporaryDirectory :: IO OsPath
getTemporaryDirectory = fromMaybe (os "/tmp") <$> lookupEnvOs (os "TMPDIR")

lookupEnvOs :: OsString -> IO (Maybe OsString)
lookupEnvOs (OsString name) = (OsString <$>) <$> Posix.getEnv name

fileTypeFromMetadata :: Metadata -> FileType
fileTypeFromMetadata stat
  | isLink    = SymbolicLink
  | isDir     = Directory
  | otherwise = File
  where
    isLink = Posix.isSymbolicLink stat
    isDir  = Posix.isDirectory stat

filesAlwaysRemovable :: Bool
filesAlwaysRemovable = True

foreign import capi "sys/stat.h fchmodat" c_fchmodat
  :: Posix.Fd -> CString -> Posix.FileMode -> CInt -> IO CInt

c_AT_FDCWD :: Posix.Fd
c_AT_FDCWD = Posix.Fd (#const AT_FDCWD)

setModeAt :: Maybe RawHandle -> OsPath -> Mode -> IO ()
setModeAt dir (OsString path) mode = do
  Posix.withFilePath path $ \ pPath ->
    Posix.throwErrnoPathIfMinus1_ "fchmodat" path $ do
      c_fchmodat (fromMaybe c_AT_FDCWD dir) pPath mode 0

setForceRemoveMode :: Mode -> Mode
setForceRemoveMode m = m .|. Posix.ownerModes

modeFromMetadata :: Metadata -> Mode
modeFromMetadata = Posix.fileMode

foreign import capi "sys/stat.h fstatat" c_fstatat
  :: Posix.Fd -> CString -> Ptr Posix.CStat -> CInt -> IO CInt

getMetadataAt :: WhetherFollow -> Maybe RawHandle -> OsPath -> IO Metadata
getMetadataAt whetherFollow dir (OsString path) =
  Posix.withFilePath path $ \ pPath -> do
    stat <- mallocForeignPtrBytes (#const sizeof(struct stat))
    withForeignPtr stat $ \ pStat -> do
      Posix.throwErrnoPathIfMinus1_ "fstatat" path $ do
        c_fstatat (fromMaybe c_AT_FDCWD dir) pPath pStat flags
    pure (Posix.FileStatus stat)
  where
    flags = atWhetherFollow whetherFollow

c_AT_SYMLINK_NOFOLLOW :: CInt
c_AT_SYMLINK_NOFOLLOW = (#const AT_SYMLINK_NOFOLLOW)

atWhetherFollow :: WhetherFollow -> CInt
atWhetherFollow NoFollow    = c_AT_SYMLINK_NOFOLLOW
atWhetherFollow FollowLinks = 0

defaultOpenFlags :: Posix.OpenFileFlags
defaultOpenFlags =
  Posix.defaultFileFlags
  { Posix.noctty = True
  , Posix.nonBlock = True
  , Posix.cloexec = True
  }

openRaw :: WhetherFollow -> Maybe RawHandle -> OsPath -> IO RawHandle
openRaw whetherFollow dir (OsString path) =
  Posix.openFdAt dir path Posix.ReadOnly flags
  where
    flags = defaultOpenFlags { Posix.nofollow = isNoFollow whetherFollow }

closeRaw :: RawHandle -> IO ()
closeRaw = Posix.closeFd

readDirStreamToEnd :: Posix.DirStream -> IO [OsPath]
readDirStreamToEnd stream = loop id
  where
    loop acc = do
      e <- Posix.readDirStream stream
      if e == mempty
        then pure (acc [])
        else loop (acc . (OsString e :))

readDirToEnd :: RawHandle -> IO [OsPath]
readDirToEnd fd =
  bracket (openDirFromFd fd) Posix.closeDirStream readDirStreamToEnd

openDirFromFd :: Posix.Fd -> IO Posix.DirStream
openDirFromFd fd = Posix.unsafeOpenDirStreamFd =<< Posix.dup fd

removePathAt :: FileType -> Maybe RawHandle -> OsPath -> IO ()
removePathAt ty dir (OsString path) =
  Posix.withFilePath path $ \ pPath -> do
    Posix.throwErrnoPathIfMinus1_ "unlinkat" path
      (c_unlinkat (fromMaybe c_AT_FDCWD dir) pPath flag)
    pure ()
  where
    flag | fileTypeIsDirectory ty = (#const AT_REMOVEDIR)
         | otherwise              = 0

getSymbolicLinkMetadata :: OsPath -> IO Metadata
getSymbolicLinkMetadata = Posix.getSymbolicLinkStatus . getOsString

foreign import ccall "unistd.h unlinkat" c_unlinkat
  :: Posix.Fd -> CString -> CInt -> IO CInt
#endif


rightOrError :: Exception e => Either e a -> a
rightOrError (Left e)  = error (displayException e)
rightOrError (Right a) = a

-- | Fallibly converts String to OsString. Only intended to be used on literals.
os :: String -> OsString
os = rightOrError . encodeUtf

data FileType = File
              | SymbolicLink -- ^ POSIX: either file or directory link; Windows: file link
              | Directory
              | DirectoryLink -- ^ Windows only: directory link
              deriving (Bounded, Enum, Eq, Ord, Read, Show)

fileTypeIsDirectory :: FileType -> Bool
fileTypeIsDirectory Directory     = True
fileTypeIsDirectory DirectoryLink = True
fileTypeIsDirectory _             = False

-- | Return whether the given 'FileType' is a link.
fileTypeIsLink :: FileType -> Bool
fileTypeIsLink SymbolicLink  = True
fileTypeIsLink DirectoryLink = True
fileTypeIsLink _             = False

type Preremover = Maybe RawHandle -> OsPath -> Metadata -> IO ()

noPreremover :: Preremover
noPreremover _ _ _ = pure ()

forcePreremover :: Preremover
forcePreremover dir path metadata = do
  when (fileTypeIsDirectory (fileTypeFromMetadata metadata)
        || not filesAlwaysRemovable) $ do
    setModeAt dir path mode
      `catchIOError` \ _ -> pure ()
  where
    mode = setForceRemoveMode (modeFromMetadata metadata)

removeRecursivelyAt
  :: (IO () -> IO ())
  -> ([IO ()] -> IO ())
  -> Preremover
  -> Maybe RawHandle
  -> OsPath
  -> IO ()
removeRecursivelyAt catcher sequencer preremover dir name = catcher $ do
  metadata <- getMetadataAt NoFollow dir name
  preremover dir name metadata
  let
    fileType = fileTypeFromMetadata metadata
    subremovals = do
      when (fileType == Directory) $ do
        bracket (openRaw NoFollow dir name) closeRaw $ \ handle -> do
          -- dropSpecialDotDirs is extremely important! Otherwise it will
          -- recurse into the parent directory and wreak havoc.
          names <- dropSpecialDotDirs <$> readDirToEnd handle
          sequencer (recurse (Just handle) <$> names)
  sequencer [subremovals, removePathAt fileType dir name]
  where recurse = removeRecursivelyAt catcher sequencer preremover

-- | @'removeDirectoryRecursive' dir@ removes an existing directory /dir/
-- together with its contents and subdirectories. Within this directory,
-- symbolic links are removed without affecting their targets.
--
-- On Windows, the operation fails if /dir/ is a directory symbolic link.
--
-- This operation is reported to be flaky on Windows so retry logic may
-- be advisable. See: https://github.com/haskell/directory/pull/108
removeDirectoryRecursive :: OsPath -> IO ()
removeDirectoryRecursive path = do
    m <- getSymbolicLinkMetadata path
    case fileTypeFromMetadata m of
      Directory ->
        removeRecursivelyAt id sequenceA_ noPreremover Nothing path
      DirectoryLink ->
        ioError (err `ioeSetErrorString` "is a directory symbolic link")
      _ ->
        ioError (err `ioeSetErrorString` "not a directory")
  where err = mkIOError InappropriateType "" Nothing Nothing

data WhetherFollow = NoFollow | FollowLinks deriving (Show)

isNoFollow :: WhetherFollow -> Bool
isNoFollow NoFollow    = True
isNoFollow FollowLinks = False

dropSpecialDotDirs :: [OsPath] -> [OsPath]
dropSpecialDotDirs = filter f
  where f filename = filename /= os "." && filename /= os ".."

-- | Fallibly converts OsString to String. Only intended to be used on literals.
so :: OsString -> String
so = rightOrError . decodeUtf

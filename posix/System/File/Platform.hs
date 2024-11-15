{-# LANGUAGE CPP              #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PackageImports   #-}

module System.File.Platform where

import Data.Either (fromRight)
import Control.Exception (try, onException, SomeException)
import GHC.IO.Handle.FD (fdToHandle')
import System.IO (IOMode(..), Handle)
import System.Posix.Types (Fd(..))
import System.Posix.IO.PosixString
    ( defaultFileFlags,
      openFd,
      closeFd,
      OpenFileFlags(noctty, nonBlock, creat, append, trunc, cloexec, exclusive),
      OpenMode(ReadWrite, ReadOnly, WriteOnly) )
import System.OsPath.Posix ( PosixPath, PosixString, (</>) )
import qualified System.OsPath.Posix as PS
import Data.IORef (IORef, newIORef)
import System.Posix (CMode)
import System.IO (utf8, latin1)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Internals (c_getpid)
import GHC.IORef (atomicModifyIORef'_)
import Foreign.C (getErrno, eEXIST, errnoToIOError)

#if !MIN_VERSION_filepath(1, 5, 0)
import Data.Coerce (coerce)
import "filepath" System.OsString.Internal.Types (PosixString(..), PosixChar(..))
import qualified "filepath" System.OsPath.Data.ByteString.Short as BC
#endif
import System.CPUTime (cpuTimePrecision, getCPUTime)
import Text.Printf (printf)

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
    fp'  <- fromRight (fmap PS.toChar . PS.unpack $ fp) <$> try @SomeException (PS.decodeFS fp)
    fdToHandle' fd Nothing False fp' iomode True

openFileWithCloseOnExec :: PosixPath -> IOMode -> IO Handle
openFileWithCloseOnExec = openFile_ defaultFileFlags' { cloexec = True }

openExistingFileWithCloseOnExec :: PosixPath -> IOMode -> IO Handle
openExistingFileWithCloseOnExec = openExistingFile_ defaultExistingFileFlags { cloexec = True }

defaultFileFlags' :: OpenFileFlags
defaultFileFlags' = defaultFileFlags { noctty = True, nonBlock = True }

defaultExistingFileFlags :: OpenFileFlags
defaultExistingFileFlags = defaultFileFlags { noctty = True, nonBlock = True, creat = Nothing }

findTempName :: (PosixString, PosixString)
             -> String
             -> PosixPath
             -> CMode
             -> IO (PosixPath, Handle)
findTempName (prefix, suffix) loc tmp_dir mode = go
 where
  go = do
    rs <- rand_string
    let filename = prefix <> rs <> suffix
        filepath = tmp_dir </> filename
    fd <- openTempFile_ filepath mode
    if fd < 0
    then do
      errno <- getErrno
      case errno of
          _ | errno == eEXIST -> go
          _ -> do
            let tmp_dir' = lenientDecode tmp_dir
            ioError (errnoToIOError loc errno Nothing (Just tmp_dir'))
    else fmap (filepath,) $ fdToHandle_ ReadWriteMode filepath fd

  openTempFile_ :: PosixPath -> CMode -> IO Fd
  openTempFile_ fp cmode = openFd fp ReadWrite defaultFileFlags' { creat = Just cmode, nonBlock = True, noctty = True, exclusive = True }

tempCounter :: IORef Int
tempCounter = unsafePerformIO $ newIORef 0
{-# NOINLINE tempCounter #-}

-- build large digit-alike number
rand_string :: IO PosixString
rand_string = do
  r1 <- fromIntegral @_ @Int <$> c_getpid
  (r2, _) <- atomicModifyIORef'_ tempCounter (+1)
  r3 <- (`quot` cpuTimePrecision) <$> getCPUTime
  return $ PS.pack $ fmap (PS.unsafeFromChar) (printf "%x-%x-%x" r1 r2 r3)

lenientDecode :: PosixString -> String
lenientDecode ps = let utf8' = PS.decodeWith utf8 ps
                       latin1' = PS.decodeWith latin1 ps
                   in case (utf8', latin1') of
                        (Right s, ~_) -> s
                        (_, Right s) -> s
                        (Left _, Left _) -> error "lenientDecode: failed to decode"

#if !MIN_VERSION_filepath(1, 5, 0)

any_ :: (PosixChar -> Bool) -> PosixString -> Bool
any_ = coerce BC.any

#endif


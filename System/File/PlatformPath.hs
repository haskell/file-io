module System.File.PlatformPath where


import System.IO (IOMode(..), Handle)
import System.OsPath.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified System.File.OsPath as OsPath
import System.OsString.Internal.Types

import Data.Coerce (coerce)

-- | Like `OsPath.openBinaryFile`, but takes a `PlatformPath` instead of an `OsPath`.
openBinaryFile :: PlatformPath -> IOMode -> IO Handle
openBinaryFile = OsPath.openBinaryFile . coerce

-- | Like `OsPath.withFile`, but takes a `PlatformPath` instead of an `OsPath`.
withFile :: PlatformPath -> IOMode -> (Handle -> IO r) -> IO r
withFile = OsPath.withFile . coerce

-- | Like `OsPath.withBinaryFile`, but takes a `PlatformPath` instead of an `OsPath`.
withBinaryFile :: PlatformPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = OsPath.withBinaryFile . coerce

-- | Like `OsPath.withFile'`, but takes a `PlatformPath` instead of an `OsPath`.
withFile' :: PlatformPath -> IOMode -> (Handle -> IO r) -> IO r
withFile' = OsPath.withFile' . coerce

-- | Like `OsPath.withBinaryFile'`, but takes a `PlatformPath` instead of an `OsPath`.
withBinaryFile' :: PlatformPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile' = OsPath.withBinaryFile' . coerce

-- | Like `OsPath.readFile`, but takes a `PlatformPath` instead of an `OsPath`.
readFile :: PlatformPath -> IO BSL.ByteString
readFile = OsPath.readFile . coerce

-- | Like `OsPath.readFile'`, but takes a `PlatformPath` instead of an `OsPath`.
readFile' :: PlatformPath -> IO BS.ByteString
readFile' = OsPath.readFile' . coerce

-- | Like `OsPath.writeFile`, but takes a `PlatformPath` instead of an `OsPath`.
writeFile :: PlatformPath -> BSL.ByteString -> IO ()
writeFile = OsPath.writeFile . coerce

-- | Like `OsPath.writeFile'`, but takes a `PlatformPath` instead of an `OsPath`.
writeFile' :: PlatformPath -> BS.ByteString -> IO ()
writeFile' = OsPath.writeFile' . coerce

-- | Like `OsPath.appendFile`, but takes a `PlatformPath` instead of an `OsPath`.
appendFile :: PlatformPath -> BSL.ByteString -> IO ()
appendFile = OsPath.appendFile . coerce

-- | Like `OsPath.appendFile'`, but takes a `PlatformPath` instead of an `OsPath`.
appendFile' :: PlatformPath -> BS.ByteString -> IO ()
appendFile' = OsPath.appendFile' . coerce

-- | Like `OsPath.openFile`, but takes a `PlatformPath` instead of an `OsPath`.
openFile :: PlatformPath -> IOMode -> IO Handle
openFile = OsPath.openFile . coerce

-- | Like `OsPath.openExistingFile`, but takes a `PlatformPath` instead of an `OsPath`.
openExistingFile :: PlatformPath -> IOMode -> IO Handle
openExistingFile = OsPath.openExistingFile . coerce

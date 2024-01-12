{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

import Test.Tasty
import Test.Tasty.HUnit
import qualified System.File.PlatformPath as PFP
import System.IO
import System.IO.Temp

import Control.Exception (bracketOnError)
import Data.Bits
import System.OsPath.Windows ( WindowsPath, pstr )
import qualified System.OsPath.Windows as WS

import qualified System.Win32 as Win32
import qualified System.Win32.WindowsString.File as WS
import Control.Monad (when, void)
#if defined(__IO_MANAGER_WINIO__)
import GHC.IO.SubSystem
#endif

-- Test that we can read concurrently without file lock
-- https://github.com/hasufell/file-io/issues/15
main :: IO ()
main = withSystemTempDirectory "tar-test" $ \baseDir' -> do
  baseDir <- WS.encodeFS baseDir'
  PFP.writeFile (baseDir WS.</> [pstr|foo|]) ""
  defaultMain $ testGroup "All"
    [ testGroup "System.File.OsPath (Windows)" $
      (map (\i -> testCase ("foo (Win32 API) " <> show i) (openFile32 (baseDir WS.</> [pstr|foo|]) ReadMode >>= Win32.closeHandle)) ([0..99] :: [Int]))
    ]

openFile32 :: WindowsPath -> IOMode -> IO Win32.HANDLE
openFile32 fp iomode =
    WS.createFile
      fp
      Win32.gENERIC_READ
      Win32.fILE_SHARE_READ
      Nothing
      Win32.oPEN_EXISTING
#if defined(__IO_MANAGER_WINIO__)
      (case ioSubSystem of
        IoPOSIX -> Win32.fILE_ATTRIBUTE_NORMAL
        IoNative -> Win32.fILE_ATTRIBUTE_NORMAL .|. Win32.fILE_FLAG_OVERLAPPED
      )
#else
      Win32.fILE_ATTRIBUTE_NORMAL
#endif
      Nothing

#else

main :: IO ()
main = putStrLn "Skipping test on windows"

#endif


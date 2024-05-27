{- |
Module      :  System.File.PlatformPath
Copyright   :  (c) Julian Ospald 2023-2024
License     :  BSD3

Maintainer  :  hasufell@posteo.de
Stability   :  stable
Portability :  portable

This module is only interesting when you are implementing low-level libraries
based on 'OsPath' API.

Usually you want "System.File.OsPath".
-}
module System.File.PlatformPath (
  openBinaryFile
, withFile
, withBinaryFile
, withFile'
, withBinaryFile'
, readFile
, readFile'
, writeFile
, writeFile'
, appendFile
, appendFile'
, openFile
, openExistingFile
) where

import System.File.PlatformPath.Internal
import Prelude ()


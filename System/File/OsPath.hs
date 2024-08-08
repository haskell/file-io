{- |
Module      :  System.File.OsPath
Copyright   :  (c) Julian Ospald 2023-2024
License     :  BSD3

Maintainer  :  hasufell@posteo.de
Stability   :  stable
Portability :  portable

This module mimics base API wrt file IO, but using 'OsPath'.
-}
module System.File.OsPath (
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
, openTempFile
, openBinaryTempFile
, openTempFileWithDefaultPermissions
, openBinaryTempFileWithDefaultPermissions
) where


import System.File.OsPath.Internal
import Prelude ()


# Revision history for file-io

## 0.1.5 -- 2024-11-26

* Don't use QuasiQotes
* Remove redundant imports

## 0.1.4 -- 2024-08-10

* fix build on GHC <= 8.10

## 0.1.3 -- 2024-08-08

* add `openTempFile` , `openBinaryTempFile` , `openTempFileWithDefaultPermissions` and `openBinaryTempFileWithDefaultPermissions` wrt [#2](https://github.com/hasufell/file-io/issues/2)

## 0.1.2 -- 2024-05-27

* expose internals via `.Internal` modules
* add `openFileWithCloseOnExec` and `openExistingFileWithCloseOnExec` to `.Internal` modules wrt [#21](https://github.com/hasufell/file-io/issues/21)

## 0.1.1 -- 2024-01-20

* fix a severe bug on windows, where `readFile` may create a missing file, wrt [#14](https://github.com/hasufell/file-io/issues/14)
* fix a concurrency bug on windows with `readFile`, wrt [#15](https://github.com/hasufell/file-io/issues/15)
* make sure to set `ioe_filename` in `IOException` wrt [#17](https://github.com/hasufell/file-io/issues/17)
* Make `openFile` and friends exception safe wrt [#8](https://github.com/hasufell/file-io/issues/8)

## 0.1.0.2 -- 2023-12-11

* support `os-string` package and newer `filepath`

## 0.1.0.1 -- YYYY-mm-dd

* Don't use creat flag when only reading files

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

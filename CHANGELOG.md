# Revision history for file-io

## 0.1.1 -- 2024-01-14

* fix a severe bug on windows, where `readFile` may create a missing file, wrt [#14](https://github.com/hasufell/file-io/issues/14)
* fix a concurrency bug on windows with `readFile`, wrt [#15](https://github.com/hasufell/file-io/issues/15)

## 0.1.0.2 -- 2023-12-11

* support `os-string` package and newer `filepath`

## 0.1.0.1 -- YYYY-mm-dd

* Don't use creat flag when only reading files

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

# `phatsort-haskell` `0.5.0.1` Release Notes

Date
: 2022-03-02

## Overview

The PhatSort project provides two command-line utilities for sorting files and
directories on FAT filesystems.  The `phatsort` utility sorts files and
directories that are already on the filesystem.  The `seqcp` utility copies
files and directories to the filesystem in sorted order.

See the [README][] for details.

[README]: <https://github.com/ExtremaIS/phatsort-haskell#readme>

## This Release

This is a patch release that makes updates to the package infrastructure.  The
package is now published to [Hackage][] and [Stackage][], and a dependency
version upper bound has been bumped.  There are no changes to the API or CLI.

[Hackage]: <https://hackage.haskell.org/package/phatsort>
[Stackage]: <https://stackage.org/package/phatsort>

### Dependency Versions

The following dependency version upper bound has been bumped to support the
latest version.

* [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative)

### Compatibility

PhatSort is currently tested with [GHC 8.2.2][] through [GHC 9.2.1][].  The
`.cabal` file uses Cabal version 1.24 (included with GHC 8.2.2), so it should
build fine on relatively old Haskell installations as well as current
installations.

[GHC 8.2.2]: <https://www.haskell.org/ghc/download_ghc_8_2_2.html>
[GHC 9.2.1]: <https://www.haskell.org/ghc/download_ghc_9_2_1.html>

### Issues

There are no known issues at this time.

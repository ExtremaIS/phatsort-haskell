# `phatsort-haskell` `0.6.0.0` Release Notes

Date
: 2023-05-28

## Overview

The PhatSort project provides two command-line utilities for sorting files and
directories on FAT filesystems.  The `phatsort` utility sorts files and
directories that are already on the filesystem.  The `seqcp` utility copies
files and directories to the filesystem in sorted order.

See the [README][] for details.

[README]: <https://github.com/ExtremaIS/phatsort-haskell#readme>

## This Release

This release adds compatibility with the latest version of the
`optparse-applicative` library.  Both lower and upper bounds of dependencies
are now tested in CI.  This release also includes changes to the project
management infrastructure.

There are no changes to the API or CLI.

### Compatibility

Build software:

| Software          | `phatsort` 0.5.0.1  | `phatsort` 0.6.0.0  |
| ----------------- | ------------------- | ------------------- |
| [GHC][]           | 8.2.2 ~ 9.2.1       | 8.2.2 ~ 9.4.5       |
| [cabal-install][] | 1.24 ~ 3.4          | 1.24 ~ 3.10         |

Note that support for GHC 9.6 is not available yet because a library used for
testing is not compatible with some GHC 9.6 boot libraries yet.

Library dependencies:

| Package            | `phatsort` 0.5.0.1 | `phatsort` 0.6.0.0  |
| ------------------ | ------------------ | ------------------- |
| [base][]           | `>=4.7 && <5`      | `>=4.10.1 && <4.18` |
| [directory][]      | `>=1.3 && <1.4`    | `>=1.3.0.2 && <1.4` |
| [filepath][]       | `>=1.4 && <1.5`    | `>=1.4.1.2 && <1.5` |
| [MonadRandom][]    | `>=0.5 && <0.6`    | `>=0.5 && <0.7`     |
| [random-shuffle][] | `>=0.0.4 && <0.1`  | `>=0.0.4 && <0.1`   |
| [transformers][]   | `>=0.5 && <0.6`    | `>=0.5 && <0.7`     |
| [unix-compat][]    | `>=0.5 && <0.6`    | `>=0.5 && <0.8`     |

Executable dependencies:

| Package                  | `phatsort` 0.5.0.1 | `phatsort` 0.6.0.0 |
| ------------------------ | ------------------ | ------------------ |
| [ansi-wl-pprint][]       | `>=0.6 && <0.7`    | `>=0.6.8 && <1.1`  |
| [optparse-applicative][] | `>=0.14 && <0.18`  | `>=0.13 && <0.19`  |
| [prettyprinter][]        |                    | `>=1.7.1 && <1.8`  |

Test dependencies:

| Package          | `phatsort` 0.5.0.1 | `phatsort` 0.6.0.0 |
| ---------------- | ------------------ | ------------------ |
| [HMock][]        | `>=0.5.1 && <0.6`  | `>=0.5.1 && <0.6`  |
| [tasty][]        | `>=1.0 && <1.5`    | `>=0.12 && <1.5`   |
| [tasty-hunit][]  | `>=0.10 && <0.11`  | `>=0.8 && <0.11`   |

To use this release with a Stackage snapshot that does not include it, add
the following to your `stack.yaml` configuration:

```yaml
extra-deps:
  - phatsort-0.6.0.0
```

[GHC]: <https://www.haskell.org/ghc/>
[cabal-install]: <https://hackage.haskell.org/package/cabal-install>
[base]: <https://hackage.haskell.org/package/base>
[directory]: <https://hackage.haskell.org/package/directory>
[filepath]: <https://hackage.haskell.org/package/filepath>
[MonadRandom]: <https://hackage.haskell.org/package/MonadRandom>
[random-shuffle]: <https://hackage.haskell.org/package/random-shuffle>
[transformers]: <https://hackage.haskell.org/package/transformers>
[unix-compat]: <https://hackage.haskell.org/package/unix-compat>
[ansi-wl-pprint]: <https://hackage.haskell.org/package/ansi-wl-pprint>
[optparse-applicative]: <https://hackage.haskell.org/package/optparse-applicative>
[prettyprinter]: <https://hackage.haskell.org/package/prettyprinter>
[HMock]: <https://hackage.haskell.org/package/HMock>
[tasty]: <https://hackage.haskell.org/package/tasty>
[tasty-hunit]: <https://hackage.haskell.org/package/tasty-hunit>
[transformers]: <https://hackage.haskell.org/package/transformers>

### Issues

There are no known issues at this time.

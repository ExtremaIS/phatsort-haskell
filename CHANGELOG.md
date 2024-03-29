# `phatsort-haskell` Changelog

This project follows the [Haskell package versioning policy][PVP], with
versions in `A.B.C.D` format.  `A` may be incremented arbitrarily for
non-technical reasons, but [semantic versioning][SemVer] is otherwise
followed, where `A.B` is the major version, `C` is the minor version, and `D`
is the patch version.  Initial development uses versions `0.0.0.D`, for which
every version is considered breaking.

[PVP]: <https://pvp.haskell.org/>
[SemVer]: <https://semver.org/>

The format of this changelog is based on [Keep a Changelog][KaC], with the
following conventions:

* Level-two heading `Unreleased` is used to track changes that have not been
  released.
* Other level-two headings specify the release in `A.B.C.D (YYYY-MM-DD)`
  format, with newer versions above older versions.
* Level-three headings are used to categorize changes as follows:
    1. Breaking
    2. Non-Breaking
* Changes are listed in arbitrary order and present tense.

[KaC]: <https://keepachangelog.com/en/1.0.0/>

## 0.6.0.0 (2023-05-28)

### Breaking

* Add support for `optparse-applicative` `0.18`

### Non-Breaking

* Bump `ansi-wl-pprint` dependency version upper bound
* Bump `MonadRandom` dependency version upper bound
* Bump `transformers` dependency version upper bound
* Bump `unix-compat` dependency version upper bound
* Adjust dependency constraints to match tested versions

## 0.5.0.1 (2022-03-02)

### Non-Breaking

* Bump `optparse-applicative` dependency version upper bound

## 0.5.0.0 (2021-12-10)

### Breaking

* Add `seqcp`
* Check that each target directory is not a mount point
* Call `sync` system call via FFI instead of running the `sync` command

### Non-Breaking

* Add tests using mocking

## 0.4.0.0 (2021-06-25)

### Breaking

* Fix `--help` when using `optparse-applicative` `0.16`

### Non-Breaking

* Refactor Nix configuration

## 0.3.0.0 (2021-05-27)

### Breaking

* Add support for `optparse-applicative` `0.16`

### Non-Breaking

* Add `.deb` and `.rpm` packaging
* Add Cabal support to `Makefile`
* Add Cabal tests to GitHub Actions
* Add [stan](https://hackage.haskell.org/package/stan) static analysis

## 0.2.0.2 (2020-11-23)

### Non-Breaking

* Use GitHub Actions instead of Travis CI

## 0.2.0.1 (2020-11-08)

### Non-Breaking

* Rename Git default branch to `main`

## 0.2.0.0 (2020-07-26)

### Breaking

* Add syncing, `--no-sync` option

### Non-Breaking

* Refactor `Makefile`, add `STACK_NIX_PATH` support
* Add `test-all` command to `Makefile`
* Add Nix configuration

## 0.1.0.3 (2019-12-22)

### Non-Breaking

* Switch back to using `LibOA` instead of `optparse-applicative-supplement`

## 0.1.0.2 (2019-12-22)

### Non-Breaking

* Use `optparse-applicative-supplement`

## 0.1.0.1 (2019-12-22)

### Non-Breaking

* Add Travis CI
* Turn off threading

## 0.1.0.0 (2019-12-21)

### Breaking

* Initial public release

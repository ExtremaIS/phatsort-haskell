# `phatsort-haskell` `0.5.0.0` Release Notes

Date
: 2021-12-10

## Overview

This release of PhatSort is almost a complete rewrite.  It is now implemented
in a way that allows for extensive testing using mocks.  There are a few
changes to the `phatsort` utility, and a new `seqcp` utility is added.
[Nix][] configuration has been removed.

[Nix]: <https://nixos.org/>

### `phatsort` Changes

The logic for checking targets has changed.  The utility now checks that a
target is not a mount point by comparing the filesystem ID of a target with
that of the parent directory.

The `sync` functionality, used to write filesystem buffers to the storage
media, is now done by calling the system call via FFI instead of running a
command.  Child processes are no longer spawned.

### `seqcp`

The `seqcp` utility sequentially copies files and directories in sorted order.
It has the same options as `phatsort` and is passed arguments like the `cp`
command: one or more source files/directories followed by a destination
directory.  Directories are copied recursively.

### Nix Configuration

The project now requires conditional compilation to build mock tests only when
using a version of GHC that the mock library works with, as well as to only
build the `sync` system call module on POSIX operating systems.  The Nix
configuration made use of [cabal2nix][], which does not have good support for
conditional compilation.  The Nix configuration has been removed until a
solution can be found.

[cabal2nix]: <https://github.com/NixOS/cabal2nix>

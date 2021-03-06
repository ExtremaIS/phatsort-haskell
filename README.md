# PhatSort

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub CI](https://github.com/ExtremaIS/phatsort-haskell/workflows/CI/badge.svg?branch=main)](https://github.com/ExtremaIS/phatsort-haskell/actions)

* [Overview](#overview)
* [Requirements](#requirements)
* [Installation](#installation)
    * [Installation From Source](#installation-from-source)
    * [`.deb` Package Installation](#deb-package-installation)
    * [`.rpm` Package Installation](#rpm-package-installation)
* [Usage](#usage)
* [Related Software](#related-software)
* [Project](#project)
    * [Links](#links)
    * [Releases](#releases)
    * [Contribution](#contribution)
    * [License](#license)

## Overview

PhatSort is a utility that sorts files and directories on a FAT filesystem.

There are many MP3 players that allow you to mount the device as external
storage and manage the media yourself.  The storage generally uses a FAT
filesystem.  When copying multiple files onto the storage, they are often
stored in an arbitrary order.  This is true when using the command line (`cp`
or `mv`) as well as when using a GUI.  This in itself is not a problem, but
the firmware of many MP3 players uses the order in FAT tables, without any
sorting.  This results in podcasts and album tracks being played out of order.

There are some utilities that sort the FAT tables of an unmounted filesystem.
(See [Related Software](#related-software) for information and links.)
Unfortunately, there are many devices for which this does not work.

PhatSort takes a different approach to solving the problem.  It works by
creating new directories and moving ("renaming") the files in the desired
order, while the filesystem is mounted.  This method works on all devices that
have been tried so far.

## Requirements

PhatSort has only been tested on Linux.  It *might* work on Windows and macOS.
Scripts that are output use POSIX shell commands and therefore require a POSIX
shell to execute.

## Installation

### Installation From Source

PhatSort can be built from source using [Stack][].  For example, you can
install the latest release (to `/usr/bin` on Linux) as follows:

```
$ git clone https://github.com/ExtremaIS/phatsort-haskell.git
$ cd phatsort-haskell
$ make
$ sudo make install
```

[Stack]: <https://www.haskellstack.org>

#### `.deb` Package Installation

Check the [Releases][] page for `.deb` packages.

#### `.rpm` Package Installation

Check the [Releases][] page for `.rpm` packages.

[Releases]: <https://github.com/ExtremaIS/phatsort-haskell/releases>

## Usage

See the [`phatsort` man page](doc/phatsort.1.md) for usage information.

## Related Software

[`FATSort`](https://fatsort.sourceforge.io/) is a command-line utility that
sorts unmounted FAT filesystems by direct manipulation of the FAT tables.
Unfortunately, there are many devices for which this does not work.

[`YAFS`](http://www.luisrios.eti.br/public/en_us/projects/yafs/) is a
command-line utility that sorts unmounted FAT filesystems by direct
manipulation of the FAT tables.
[`Visual YAFS`](http://www.luisrios.eti.br/public/en_us/projects/visual_yafs/)
provides a GUI.  I have not tried either of these.

[`DriveSort`](http://www.anerty.net/software/file/DriveSort/) is Windows GUI
software that sorts unmounted FAT filesystems by direct manipulation of the
FAT tables.  I have not tried it.

## Project

PhatSort was written quickly to solve a particular pain point.  There are no
plans to expose a library or put the package on Hackage.

### Links

* GitHub: <https://github.com/ExtremaIS/phatsort-haskell>

### Releases

All releases are tagged in the `main` branch.  Release tags are signed using
the
[`security@extrema.is` GPG key](http://keys.gnupg.net/pks/lookup?op=vindex&fingerprint=on&search=0x1D484E4B4705FADF).

### Contribution

Issues and feature requests are tracked on GitHub:
<https://github.com/ExtremaIS/phatsort-haskell/issues>

Issues may also be submitted via email to <bugs@extrema.is>.

### License

This project is released under the
[MIT License](https://opensource.org/licenses/MIT) as specified in the
[`LICENSE`](LICENSE) file.

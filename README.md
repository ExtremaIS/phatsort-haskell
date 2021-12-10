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

The PhatSort project provides two command-line utilities for sorting files
and directories on FAT filesystems.  The `phatsort` utility sorts files and
directories that are already on the filesystem.  The `seqcp` utility copies
files and directories to the filesystem in sorted order.

There are many MP3 players that allow you to mount the device as external
storage and manage the media yourself.  The storage generally uses a FAT
filesystem.  When copying multiple files onto the storage, using the command
line (`cp`/`mv`) or a GUI, they are generally stored in an arbitrary order.
This is not a problem if the firmware of the MP3 player sorts by filename, but
many MP3 players use the order in the FAT filesystem without sorting, which
results in podcasts and album tracks being played out of order.

There are some utilities that sort the FAT tables of an unmounted filesystem.
(See [Related Software](#related-software) for information and links.)
Unfortunately, there are many devices for which this does not work.  PhatSort
takes a different approach to solving the problem.  It works by creating new
directories and moving ("renaming") the files in the desired order, while the
filesystem is mounted.  This method works on all devices that have been tried
so far.

PhatSort also (optionally) forces the filesystem buffers to be written to the
storage media after each change.  This helps avoid write failures when using
devices that have problems with writing large amounts of data.  Note that the
`seqcp` utility helps with this issue even on non-FAT filesystems.  For
example, some Android devices mounted using MTP have this issue.

## Requirements

PhatSort has only been tested on Linux.  It *might* work on other operating
systems.  Scripts that are output use POSIX shell commands and therefore
require a POSIX shell to execute.

## Installation

### Installation From Source

PhatSort can be built from source using [Stack][].  For example, you can
install the latest release (to `/usr/local` on Linux) as follows:

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

See the [phatsort](doc/phatsort.1.md) and [seqcp](doc/seqcp.1.md) man pages
for usage information.

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
plans put the package on Hackage.

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

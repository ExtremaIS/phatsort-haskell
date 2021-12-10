---
title: SEQCP
section: 1
hyphenate: false
...

# NAME

`seqcp` - sequentially copy files and directories

# SYNOPSIS

`seqcp` [*OPTIONS*] SOURCE ... DESTINATION

# DESCRIPTION

The `seqcp` utility copies files and directories sequentially.  This may be
done for two reasons:

* Options are used to specify the order, which determines the order of
  playback of media files on a FAT filesystem on some devices.
* Buffers are (optionally) synchronized after each copy, which helps avoid
  copy failures on some devices.

Note that directories are copied recursively.

# OPTIONS

-h, \--help
:   show help and exit

\--version
:   show version and exit

-c, \--case
:   case-insensitive sort

-f, \--first *TYPE*
:   sort certain directory entries first

    The following types are supported:

    * *dirs* - sort directories before files
    * *files* - sort files before directories

-n, \--no-sync
:   do not sync after each command

-o, \--order *ORDER*
:   desired order (default: *name*)

    The following orders are supported:

    * *name* - sort by filename
    * *time* - sort by modification time
    * *random* - random order

-r, \--reverse
:   reverse sort

-s, \--script
:   output script instead of executing

-v, \--verbose
:   display progress

# ARGUMENTS

*SOURCE*
:   source directory or file

*DESTINATION*
:   destination directory

# EXIT CODES

0
:   no error

1
:   execution error

2
:   command-line error

# PROJECT

GitHub:
:   <https://github.com/ExtremaIS/phatsort-haskell>

Reporting issues:
:   GitHub: <https://github.com/ExtremaIS/phatsort-haskell/issues>

    Email: <bugs@extrema.is>

Copyright
:   Copyright (c) 2019-2021 Travis Cardwell

License
:   The MIT License <https://opensource.org/licenses/MIT>

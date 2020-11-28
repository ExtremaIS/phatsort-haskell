---
title: PHATSORT
section: 1
hyphenate: false
...

# NAME

`phatsort` - FAT filesystem sort utility

# SYNOPSIS

`phatsort` [*OPTIONS*] TARGET ...

# DESCRIPTION

PhatSort is a utility for sorting files and directories on a FAT filesystem.

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

*TARGET*
:   target directory

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
:   Copyright (c) 2019-2020 Travis Cardwell

License
:   The MIT License <https://opensource.org/licenses/MIT>

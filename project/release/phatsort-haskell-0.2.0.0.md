# `phatsort-haskell` `0.2.0.0` Release Notes

Date
: 2020-07-26

## Overview

In this major release of PhatSort, there is one significant change: the `sync`
command is run after each command by default.  This helps prevent failures
when writes are performed too quickly on cheap NAND chips.  In cases when you
do not want to do this, the `--no-sync` option turns this feature off.

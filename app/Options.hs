------------------------------------------------------------------------------
-- |
-- Module      : Options
-- Description : CLI options and help documentation
-- Copyright   : Copyright (c) 2019-2023 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module Options where

-- https://hackage.haskell.org/package/ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen (Doc)

-- https://hackage.haskell.org/package/base
import Data.Bool (bool)

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA

-- (phatsort)
import PhatSort.SortOptions
  ( SortCase(CaseInsensitive, CaseSensitive)
  , SortFirst(FirstDirs, FirstFiles, FirstNone)
  , SortOrder(OrderName, OrderRandom, OrderTime)
  )

-- (phatsort:executable)
import qualified LibOA

------------------------------------------------------------------------------

caseOption :: OA.Parser SortCase
caseOption = fmap (bool CaseSensitive CaseInsensitive) . OA.switch $
    mconcat
      [ OA.long "case"
      , OA.short 'c'
      , OA.help "case-insensitive sort"
      ]

------------------------------------------------------------------------------

exitCodeHelp :: Doc
exitCodeHelp = LibOA.section "Exit codes:" $ LibOA.table_ 2
    [ ["0", "no error"]
    , ["1", "execution error"]
    , ["2", "command line error"]
    ]

------------------------------------------------------------------------------

firstOption :: OA.Parser SortFirst
firstOption = OA.option (OA.eitherReader parseFirst) $ mconcat
    [ OA.long "first"
    , OA.short 'f'
    , OA.metavar "TYPE"
    , OA.value FirstNone
    , OA.help "sort certain directory entries first"
    ]
  where
    parseFirst :: String -> Either String SortFirst
    parseFirst "dirs" = Right FirstDirs
    parseFirst "files" = Right FirstFiles
    parseFirst _ = Left "unknown first option; \"dirs\" or \"files\" expected"

------------------------------------------------------------------------------

firstTypeHelp :: Doc
firstTypeHelp = LibOA.section "TYPE choices:" $ LibOA.table_ 2
    [ ["dirs", "sort directories before files"]
    , ["files", "sort files before directories"]
    ]

------------------------------------------------------------------------------

noSyncOption :: OA.Parser Bool
noSyncOption = fmap not . OA.switch $ mconcat
    [ OA.long "no-sync"
    , OA.short 'n'
    , OA.help "do not sync after each command"
    ]

------------------------------------------------------------------------------

orderOption :: OA.Parser SortOrder
orderOption = OA.option (OA.eitherReader parseOrder) $ mconcat
    [ OA.long "order"
    , OA.short 'o'
    , OA.metavar "ORDER"
    , OA.value OrderName
    , OA.help "desired order (default: name)"
    ]
  where
    parseOrder :: String -> Either String SortOrder
    parseOrder "name" = Right OrderName
    parseOrder "time" = Right OrderTime
    parseOrder "random" = Right OrderRandom
    parseOrder _ = Left "unknown ORDER"

------------------------------------------------------------------------------

orderHelp :: Doc
orderHelp = LibOA.section "ORDER choices:" $ LibOA.table_ 2
    [ ["name", "sort by filename"]
    , ["time", "sort by modification time"]
    , ["random", "random order"]
    ]

------------------------------------------------------------------------------

reverseOption :: OA.Parser Bool
reverseOption = OA.switch $ mconcat
    [ OA.long "reverse"
    , OA.short 'r'
    , OA.help "reverse sort"
    ]

------------------------------------------------------------------------------

scriptOption :: OA.Parser Bool
scriptOption = OA.switch $ mconcat
    [ OA.long "script"
    , OA.short 's'
    , OA.help "output script instead of executing"
    ]

------------------------------------------------------------------------------

verboseOption :: OA.Parser Bool
verboseOption = OA.switch $ mconcat
    [ OA.long "verbose"
    , OA.short 'v'
    , OA.help "display progress"
    ]

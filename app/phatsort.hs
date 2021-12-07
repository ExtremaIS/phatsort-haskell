------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : phatsort CLI
-- Copyright   : Copyright (c) 2019-2021 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module Main (main) where

-- https://hackage.haskell.org/package/base
import Control.Applicative (some)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA

-- (phatsort)
import PhatSort (version)
import PhatSort.Cmd.PhatSort (Options(Options), runIO)

-- (phatsort:executable)
import qualified LibOA
import Options
  ( caseOption, exitCodeHelp, firstOption, firstTypeHelp, noSyncOption
  , orderHelp, orderOption, reverseOption, scriptOption, verboseOption
  )

------------------------------------------------------------------------------

options :: OA.Parser Options
options = Options
    <$> caseOption
    <*> firstOption
    <*> noSyncOption
    <*> orderOption
    <*> reverseOption
    <*> scriptOption
    <*> verboseOption
    <*> targetArguments

------------------------------------------------------------------------------

targetArguments :: OA.Parser [FilePath]
targetArguments = some . OA.strArgument $ mconcat
    [ OA.metavar "TARGET ..."
    , OA.help "target directories"
    ]

------------------------------------------------------------------------------

parseOptions :: IO Options
parseOptions = OA.execParser
    . OA.info (LibOA.helper <*> LibOA.versioner version <*> options)
    $ mconcat
        [ OA.fullDesc
        , OA.progDesc "FAT filesystem sort utility"
        , OA.failureCode 2
        , OA.footerDoc . Just $ LibOA.vspace
            [ firstTypeHelp
            , orderHelp
            , exitCodeHelp
            ]
        ]

------------------------------------------------------------------------------

errorExit :: String -> IO a
errorExit message = do
    hPutStrLn stderr $ "error: " ++ message
    exitWith $ ExitFailure 1

------------------------------------------------------------------------------

main :: IO ()
main = either errorExit pure =<< runIO =<< parseOptions

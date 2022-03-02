------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : seqcp CLI
-- Copyright   : Copyright (c) 2019-2022 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-- https://hackage.haskell.org/package/base
import Control.Applicative (some)
import Data.Bifunctor (first)
import Data.List.NonEmpty (nonEmpty)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA

-- (phatsort)
import PhatSort (version)
import PhatSort.Cmd.SeqCp
  ( Options
      ( Options, optCase, optFirst, optSync, optOrder, optReverse, optScript
      , optVerbose, optSources, optDestination
      )
  , runIO
  )
import PhatSort.SortOptions (SortCase, SortFirst, SortOrder)

-- (phatsort:executable)
import qualified LibOA
import Options
  ( caseOption, exitCodeHelp, firstOption, firstTypeHelp, noSyncOption
  , orderHelp, orderOption, reverseOption, scriptOption, verboseOption
  )

------------------------------------------------------------------------------

data Options'
  = Options'
    { optCase    :: !SortCase
    , optFirst   :: !SortFirst
    , optSync    :: !Bool
    , optOrder   :: !SortOrder
    , optReverse :: !Bool
    , optScript  :: !Bool
    , optVerbose :: !Bool
    , optArgs    :: ![FilePath]
    }
  deriving Show

------------------------------------------------------------------------------

options :: OA.Parser Options'
options = Options'
    <$> caseOption
    <*> firstOption
    <*> noSyncOption
    <*> orderOption
    <*> reverseOption
    <*> scriptOption
    <*> verboseOption
    <*> arguments

------------------------------------------------------------------------------

arguments :: OA.Parser [FilePath]
arguments = some . OA.strArgument $ mconcat
    [ OA.metavar "SOURCE ... DESTINATION"
    , OA.help "source directories/files and destination directory"
    ]

------------------------------------------------------------------------------

parseOptions :: IO Options
parseOptions = processOptions =<< OA.execParser info
  where
    info :: OA.ParserInfo Options'
    info
      = OA.info (LibOA.helper <*> LibOA.versioner version <*> options)
      $ mconcat
          [ OA.fullDesc
          , OA.progDesc "sequentially copy files and directories"
          , OA.failureCode 2
          , OA.footerDoc . Just $ LibOA.vspace
              [ firstTypeHelp
              , orderHelp
              , exitCodeHelp
              ]
          ]

    processOptions :: Options' -> IO Options
    processOptions Options'{..} = case first nonEmpty <$> unsnoc optArgs of
      Just (Just optSources, optDestination) -> pure Options{..}
      Just (Nothing, _destination) -> usageExit "Missing: DESTINATION"
      Nothing -> usageExit "Missing: SOURCE ... DESTINATION"

    unsnoc :: [FilePath] -> Maybe ([FilePath], FilePath)
    unsnoc (x:xs)
      | null xs = Just ([], x)
      | otherwise = first (x :) <$> unsnoc xs
    unsnoc [] = Nothing

    usageExit :: String -> IO a
    usageExit msg = OA.handleParseResult . OA.Failure $
      OA.parserFailure OA.defaultPrefs info (OA.ErrorMsg msg) []

------------------------------------------------------------------------------

errorExit :: String -> IO a
errorExit message = do
    hPutStrLn stderr $ "error: " ++ message
    exitWith $ ExitFailure 1

------------------------------------------------------------------------------

main :: IO ()
main = either errorExit pure =<< runIO =<< parseOptions


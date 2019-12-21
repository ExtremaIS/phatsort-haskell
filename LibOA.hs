------------------------------------------------------------------------------
-- |
-- Module      : LibOA
-- Description : optparse-applicative library functions
-- Copyright   : Copyright (c) 2018-2019 Travis Cardwell
-- License     : MIT
--
-- This is a collection of functions that I often use with
-- @optparse-applicative@.  I do not feel that it is worth creating another
-- helper package, so I currently just copy the code to different projects as
-- required.
--
-- Revision: 2019-12-21
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module LibOA
  ( -- * Parsing
    -- $Parsing
    execParser
  , customExecParser
  , handleParseResult
    -- * Options
    -- $Options
  , helper
  , versioner
    -- * Utilities
  , commands
    -- * Help
  , (<||>)
  , section
  , table
  , vspace
  ) where

-- https://hackage.haskell.org/package/ansi-wl-pprint
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import Text.PrettyPrint.ANSI.Leijen (Doc)

-- https://hackage.haskell.org/package/base
import qualified Data.List as List
#if !MIN_VERSION_base (4,11,0)
import Data.Monoid ((<>))
#endif
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA
import qualified Options.Applicative.Common as OAC
import qualified Options.Applicative.Types as OAT

------------------------------------------------------------------------------
-- $Parsing
--
-- These functions do the same thing as the standard functions of the same
-- name except that they exit with exit code 2 on error, conforming to Unix
-- conventions.

-- | Parse command-line arguments
--
-- Standard version: 'OA.execParser'
execParser ::OA.ParserInfo a -> IO a
execParser = customExecParser OA.defaultPrefs

-- | Parse command-line arguments with custom preferences
--
-- Standard version: 'OA.customExecParser'
customExecParser :: OA.ParserPrefs -> OA.ParserInfo a -> IO a
customExecParser pprefs pinfo
    = OA.execParserPure pprefs pinfo <$> getArgs
    >>= handleParseResult

-- | Handle a 'OA.ParserResult'
--
-- Standard version: 'OA.handleParseResult'
handleParseResult :: OA.ParserResult a -> IO a
handleParseResult (OA.Success x) = return x
handleParseResult (OA.Failure failure) = do
    progn <- getProgName
    let (msg, exit) = OA.renderFailure failure progn
    case exit of
      ExitSuccess -> do
        putStrLn msg
        exitSuccess
      _ -> do
        hPutStrLn stderr msg
        exitWith $ ExitFailure 2
handleParseResult (OA.CompletionInvoked compl) = do
    progn <- getProgName
    putStr =<< OA.execCompletion compl progn
    exitSuccess

------------------------------------------------------------------------------
-- $Options
--
-- Option descriptions are not capitalized

-- | A hidden @-h@ / @--help@ option that always fails, showing the help
--
-- This is the same as 'OA.helper' except that it has a different help
-- message.
helper :: OA.Parser (a -> a)
helper = OA.abortOption OA.ShowHelpText $ mconcat
    [ OA.short 'h'
    , OA.long "help"
    , OA.help "show help and exit"
    , OA.hidden
    ]

-- | A hidden @--version@ option that always fails, showing the version
versioner
  :: String  -- ^ version string
  -> OA.Parser (a -> a)
versioner verStr = OA.infoOption verStr $ mconcat
    [ OA.long "version"
    , OA.help "show version and exit"
    , OA.hidden
    ]

------------------------------------------------------------------------------
-- $Utilities

-- | Get a list of commands for a parser
commands :: OA.Parser a -> [String]
commands =
    let go _ opt = case OAT.optMain opt of
           OAT.CmdReader _ cmds _ -> reverse cmds
           _                      -> []
    in  concat . OAC.mapParser go

------------------------------------------------------------------------------
-- $Help

-- | Insert a blank line between two documents
(<||>) :: Doc -> Doc -> Doc
d1 <||> d2 = d1 <> Doc.line <> Doc.line <> d2

-- | Create a section with a title and indented body
section :: String -> Doc -> Doc
section title = (Doc.text title Doc.<$$>) . Doc.indent 2

-- | Create a two-column table
table :: [(String, String)] -> Doc
table rows =
    let width = 1 + maximum (map (length . fst) rows)
    in  Doc.vcat
          [ Doc.fillBreak width (Doc.text l) Doc.<+> Doc.text r
          | (l, r) <- rows
          ]

-- | Vertically space documents with blank lines between them
vspace :: [Doc] -> Doc
vspace = mconcat . List.intersperse (Doc.line <> Doc.line)

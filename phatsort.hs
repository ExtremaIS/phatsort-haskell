------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : PhatSort: a FAT filesystem sort utility
-- Copyright   : Copyright (c) 2019-2020 Travis Cardwell
-- License     : MIT
--
-- See the README for details.
------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-- https://hackage.haskell.org/package/ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen (Doc)

-- https://hackage.haskell.org/package/base
import Control.Applicative (liftA2, some)
import Control.Monad (forM, forM_, unless, when)
import Data.Bool (bool)
import Data.Char (ord, toLower)
import Data.Either (partitionEithers)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf, sortBy, sortOn)
import Data.Version (showVersion)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- https://hackage.haskell.org/package/directory
import qualified System.Directory as Dir

-- https://hackage.haskell.org/package/filepath
import System.FilePath ((</>), splitDirectories)

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA

-- https://hackage.haskell.org/package/random-shuffle
import System.Random.Shuffle (shuffleM)

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (runExceptT, throwE)

-- (phatsort:cabal)
import qualified Paths_phatsort as Project

-- (phatsort:executable)
import qualified LibOA

------------------------------------------------------------------------------

-- | Case sensitivity
data SortCase
  = CaseSensitive
  | CaseInsensitive
  deriving (Eq, Show)

-- | Sorting certain directory entries first
data SortFirst
  = FirstNone
  | FirstDirs
  | FirstFiles
  deriving (Eq, Show)

-- | Sort order
data SortOrder
  = OrderName
  | OrderTime
  | OrderRandom
  deriving (Eq, Show)

-- | Utility options
data Options
  = Options
    { optCase    :: !SortCase
    , optFirst   :: !SortFirst
    , optOrder   :: !SortOrder
    , optReverse :: !Bool
    , optScript  :: !Bool
    , optVerbose :: !Bool
    , optTargets :: ![FilePath]
    }
  deriving Show

-- | Internal type with details about a target directory
data Target
  = Target
    { tgtRelPath :: !FilePath
    , tgtSrcPath :: !FilePath
    , tgtDstPath :: !FilePath
    }
  deriving Show

------------------------------------------------------------------------------

-- | Utility implementation
phatsort :: Options -> IO ()
phatsort Options{..} = do
    targets <- getTargets optTargets
    forM_ targets $ \Target{..} -> do
      putProgress tgtRelPath
      mvDir tgtDstPath tgtSrcPath
      mkDir tgtDstPath
      processDir tgtRelPath tgtSrcPath tgtDstPath
      rmDir tgtSrcPath
  where
    processDir :: FilePath -> FilePath -> FilePath -> IO ()
    processDir relPath srcPath dstPath = do
      nps <- map (liftA2 (,) id (srcPath </>)) <$> Dir.listDirectory
        (if optScript then dstPath else srcPath)
      let goDir (name, srcPath') = do
            let relPath' = relPath </> name
                dstPath' = dstPath </> name
            putProgress relPath'
            mkDir dstPath'
            processDir relPath' srcPath' dstPath'
            rmDir srcPath'
          goFile (name, srcPath') = do
            let relPath' = relPath </> name
                dstPath' = dstPath </> name
            putProgress relPath'
            mvFile srcPath' dstPath'
          go np@(name, srcPath') = do
            let dstPath' = dstPath </> name
            bool (goFile np) (goDir np) =<< Dir.doesDirectoryExist
              (if optScript then dstPath' else srcPath')
          eitherDirOrFile np@(name, srcPath') = do
            let dstPath' = dstPath </> name
            bool (Right np) (Left np) <$> Dir.doesDirectoryExist
              (if optScript then dstPath' else srcPath')
          splitDirs = partitionEithers <$> mapM eitherDirOrFile nps
      case optFirst of
        FirstNone -> mapM_ go =<< doSort nps
        FirstDirs -> do
          (dnps, fnps) <- splitDirs
          mapM_ goDir =<< doSort dnps
          mapM_ goFile =<< doSort fnps
        FirstFiles -> do
          (dnps, fnps) <- splitDirs
          mapM_ goFile =<< doSort fnps
          mapM_ goDir =<< doSort dnps

    doSort :: [(FilePath, FilePath)] -> IO [(FilePath, FilePath)]
    doSort nps = case optOrder of
      OrderName -> return . doReverse $ sortBy doCompare nps
      OrderTime -> fmap (doReverse . map fst . sortOn snd) . forM nps $ \np ->
        (,) np <$> Dir.getModificationTime (snd np)
      OrderRandom -> shuffleM nps

    doReverse :: [a] -> [a]
    doReverse
      | optReverse = reverse
      | otherwise  = id

    doCompare :: (FilePath, FilePath) -> (FilePath, FilePath) -> Ordering
    doCompare (name1, _) (name2, _) = case optCase of
      CaseSensitive -> compare name1 name2
      CaseInsensitive -> compare (map toLower name1) (map toLower name2)

    mvDir :: FilePath -> FilePath -> IO ()
    mvDir srcPath dstPath
      | optScript = putCmd ["mv", srcPath, dstPath]
      | otherwise = Dir.renameDirectory srcPath dstPath

    mkDir :: FilePath -> IO ()
    mkDir path
      | optScript = putCmd ["mkdir", path]
      | otherwise = Dir.createDirectory path

    rmDir :: FilePath -> IO ()
    rmDir path
      | optScript = putCmd ["rmdir", path]
      | otherwise = Dir.removeDirectory path

    mvFile :: FilePath -> FilePath -> IO ()
    mvFile srcPath dstPath
      | optScript = putCmd ["mv", srcPath, dstPath]
      | otherwise = Dir.renameFile srcPath dstPath

    putProgress :: FilePath -> IO ()
    putProgress path
      | optScript = when optVerbose $ putCmd ["echo", path]
      | otherwise = when optVerbose $ putStrLn path

-- | Validate the target arguments
getTargets :: [FilePath] -> IO [Target]
getTargets args = do
    curDir' <- splitDirectories <$> Dir.getCurrentDirectory
    (errs, targets) <- fmap partitionEithers . forM args $ \tgtRelPath ->
      runExceptT $ do
        when ("-phat" `isSuffixOf` tgtRelPath) . throwE $
          "-phat directory: " ++ tgtRelPath
        tgtDstPath <- lift . Dir.makeAbsolute $
          dropWhileEnd (== '/') tgtRelPath
        when (splitDirectories tgtDstPath `isPrefixOf` curDir') . throwE $
          "above current directory: " ++ tgtRelPath
        isDir <- lift $ Dir.doesDirectoryExist tgtDstPath
        unless isDir $ do
          exists <- lift $ Dir.doesPathExist tgtDstPath
          throwE $ if exists
            then "not a directory: " ++ tgtRelPath
            else "not found: " ++ tgtRelPath
        let tgtSrcPath = tgtDstPath ++ "-phat"
        exists <- lift $ Dir.doesPathExist tgtSrcPath
        when exists . throwE $ "already exists: " ++ tgtRelPath ++ "-phat"
        return Target{..}
    unless (null errs) $ do
      mapM_ (hPutStrLn stderr . ("error: " ++)) errs
      exitFailure
    return targets

-- | Output a shell command, escaping it
putCmd :: [String] -> IO ()
putCmd = putStrLn . unwords . map escape
  where
    escape :: String -> String
    escape s
      | all isOkChar s = s
      | otherwise = "'" ++ concatMap tr s ++ "'"

    tr :: Char -> String
    tr '\'' = "'\"'\"'"
    tr '\0' = ""
    tr c    = [c]

    isOkChar :: Char -> Bool
    isOkChar c =
      let c' = ord c
      in  or
            [ (fromIntegral (c' - ord 'a') :: Word) <= 25
            , (fromIntegral (c' - ord 'A') :: Word) <= 25
            , (fromIntegral (c' - ord '0') :: Word) <= 9
            , c `elem` ".-+_/"
            ]

------------------------------------------------------------------------------

caseOption :: OA.Parser SortCase
caseOption = fmap (bool CaseSensitive CaseInsensitive) . OA.switch $ mconcat
    [ OA.long "case"
    , OA.short 'c'
    , OA.help "case-insensitive sort"
    ]

firstOption :: OA.Parser SortFirst
firstOption = OA.option (OA.eitherReader parse) $ mconcat
    [ OA.long "first"
    , OA.short 'f'
    , OA.metavar "TYPE"
    , OA.value FirstNone
    , OA.help "sort certain directory entries first"
    ]
  where
    parse :: String -> Either String SortFirst
    parse "dirs" = Right FirstDirs
    parse "files" = Right FirstFiles
    parse _ = Left "unknown first option; \"dirs\" or \"files\" expected"

orderOption :: OA.Parser SortOrder
orderOption = OA.option (OA.eitherReader parse) $ mconcat
    [ OA.long "order"
    , OA.short 'o'
    , OA.metavar "ORDER"
    , OA.value OrderName
    , OA.help "desired order (default: name)"
    ]
  where
    parse :: String -> Either String SortOrder
    parse "name" = Right OrderName
    parse "time" = Right OrderTime
    parse "random" = Right OrderRandom
    parse _ = Left "unknown ORDER"

reverseOption :: OA.Parser Bool
reverseOption = OA.switch $ mconcat
    [ OA.long "reverse"
    , OA.short 'r'
    , OA.help "reverse sort"
    ]

scriptOption :: OA.Parser Bool
scriptOption = OA.switch $ mconcat
    [ OA.long "script"
    , OA.short 's'
    , OA.help "output script instead of executing"
    ]

verboseOption :: OA.Parser Bool
verboseOption = OA.switch $ mconcat
    [ OA.long "verbose"
    , OA.short 'v'
    , OA.help "display progress"
    ]

targetArguments :: OA.Parser [FilePath]
targetArguments = some . OA.strArgument $ mconcat
    [ OA.metavar "TARGET ..."
    , OA.help "target directories"
    ]

options :: OA.Parser Options
options = Options
    <$> caseOption
    <*> firstOption
    <*> orderOption
    <*> reverseOption
    <*> scriptOption
    <*> verboseOption
    <*> targetArguments

main :: IO ()
main = phatsort =<< OA.execParser pinfo
  where
    pinfo :: OA.ParserInfo Options
    pinfo
      = OA.info (LibOA.helper <*> LibOA.versioner version <*> options)
      $ mconcat
          [ OA.fullDesc
          , OA.progDesc "FAT filesystem sort utility"
          , OA.failureCode 2
          , OA.footerDoc . Just $ LibOA.vspace
              [ typeHelp
              , orderHelp
              , exitCodeHelp
              ]
          ]

    version :: String
    version = "phatsort-haskell " ++ showVersion Project.version

    typeHelp :: Doc
    typeHelp = LibOA.section "TYPE choices:" $ LibOA.table
      [ ("dirs", "sort directories before files")
      , ("files", "sort files before directories")
      ]

    orderHelp :: Doc
    orderHelp = LibOA.section "ORDER choices:" $ LibOA.table
      [ ("name", "sort by filename")
      , ("time", "sort by modification time")
      , ("random", "random order")
      ]

    exitCodeHelp :: Doc
    exitCodeHelp = LibOA.section "Exit codes:" $ LibOA.table
      [ ("0", "no error")
      , ("1", "execution error")
      , ("2", "command line error")
      ]

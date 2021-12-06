{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module PhatSort.Cmd.PhatSort.HMock (tests) where

-- https://hackage.haskell.org/package/base
import GHC.Stack (HasCallStack)

-- https://hackage.haskell.org/package/dlist
import qualified Data.DList as DList
import Data.DList (DList)

-- https://hackage.haskell.org/package/HMock
import Test.HMock
  ( (|->), MockT, expect, inAnyOrder, inSequence, makeMockable, runMockT
  )

-- https://hackage.haskell.org/package/MonadRandom
import qualified Control.Monad.Random.Lazy as Rand
import Control.Monad.Random.Lazy (MonadRandom, RandT, StdGen, evalRandT)

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), assertFailure, testCase)

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.Lazy (WriterT(runWriterT), tell)

-- (phatsort)
import PhatSort.Cmd.PhatSort
  ( Options
      ( Options, optCase, optFirst, optOrder, optReverse, optScript, optSync
      , optTargets, optVerbose
      )
  , run
  )
import PhatSort.Monad.FileSystem (MonadFileSystem, FileStatus(FileStatus))
import PhatSort.Monad.Process (MonadProcess)
import PhatSort.Monad.Stdio (MonadStdio(putStrLn))
import qualified PhatSort.Monad.Trans.Error as Error
import PhatSort.SortOptions
  ( SortCase(CaseInsensitive, CaseSensitive)
  , SortFirst(FirstDirs, FirstFiles, FirstNone)
  , SortOrder(OrderName, OrderRandom, OrderTime)
  )

------------------------------------------------------------------------------

makeMockable [t|MonadFileSystem|]

makeMockable [t|MonadProcess|]

------------------------------------------------------------------------------

instance MonadRandom m => MonadRandom (MockT m) where
  getRandomR  = lift . Rand.getRandomR
  getRandom   = lift   Rand.getRandom
  getRandomRs = lift . Rand.getRandomRs
  getRandoms  = lift   Rand.getRandoms

instance Monad m => MonadStdio (MockT (WriterT (DList String) m)) where
  putStrLn = lift . tell . DList.singleton

------------------------------------------------------------------------------

runTest
  :: MockT (WriterT (DList String) (RandT StdGen IO)) (Either String ())
  -> IO (Either String [String])
runTest
    = fmap getResult
    . flip evalRandT (Rand.mkStdGen 42)
    . runWriterT
    . runMockT
  where
    getResult :: (Either String (), DList String) -> Either String [String]
    getResult (Left err, _outLines) = Left err
    getResult (Right (), outLines)  = Right $ DList.toList outLines

------------------------------------------------------------------------------

assertOutput
  :: HasCallStack
  => [String]  -- ^ actual output
  -> [String]  -- ^ expected output
  -> IO ()
assertOutput = go 1
  where
    go :: Int -> [String] -> [String] -> IO ()
    go !lineNum (actualLine:actualLines) (expectedLine:expectedLines)
      | actualLine == expectedLine =
          go (lineNum + 1) actualLines expectedLines
      | otherwise = assertFailure $ unlines
          [ "output differs (line " ++ show lineNum ++ ")"
          , "  expected: " ++ expectedLine
          , "  actual:   " ++ actualLine
          ]
    go !lineNum [] (expectedLine:_expectedLines) = assertFailure $ unlines
      [ "output too short (line " ++ show lineNum ++ ")"
      , "  expected: " ++ expectedLine
      ]
    go !lineNum (actualLine:_actualLines) [] = assertFailure $ unlines
      [ "output too long (line " ++ show lineNum ++ ")"
      , "  actual: " ++ actualLine
      ]
    go _lineNum [] [] = pure ()

------------------------------------------------------------------------------

defaultOptions :: Options
defaultOptions = Options
    { optCase    = CaseSensitive
    , optFirst   = FirstNone
    , optSync    = True
    , optOrder   = OrderName
    , optReverse = False
    , optScript  = False
    , optVerbose = False
    , optTargets = []
    }

------------------------------------------------------------------------------

testCaseSensitive :: TestTree
testCaseSensitive = testCase "CaseSensitive" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat" |->
            Right ["Uno.mp3", "dos.mp3", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one-phat/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one-phat/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one-phat/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ RenameFile "/a/b/one-phat/Uno.mp3" "/a/b/one/Uno.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        ]
      Error.run $ run defaultOptions
        { optTargets = ["one"]
        }
    assertOutput output []

testCaseSensitiveScript :: TestTree
testCaseSensitiveScript = testCase "CaseSensitiveScript" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ ListDirectory "/a/b/one" |->
            Right ["Uno.mp3", "dos.mp3", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        ]
      Error.run $ run defaultOptions
        { optScript  = True
        , optTargets = ["one"]
        }
    assertOutput output
      [ "mv /a/b/one /a/b/one-phat",                   "sync"
      , "mkdir /a/b/one",                              "sync"
      , "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3",   "sync"
      , "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3",   "sync"
      , "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3", "sync"
      , "rmdir /a/b/one-phat",                         "sync"
      ]

------------------------------------------------------------------------------

testCaseInsensitive :: TestTree
testCaseInsensitive = testCase "CaseInsensitive" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat" |->
            Right ["Uno.mp3", "dos.mp3", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one-phat/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one-phat/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one-phat/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/Uno.mp3" "/a/b/one/Uno.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        ]
      Error.run $ run defaultOptions
        { optCase    = CaseInsensitive
        , optTargets = ["one"]
        }
    assertOutput output []

testCaseInsensitiveScript :: TestTree
testCaseInsensitiveScript = testCase "CaseInsensitiveScript" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ ListDirectory "/a/b/one" |->
            Right ["Uno.mp3", "dos.mp3", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        ]
      Error.run $ run defaultOptions
        { optCase    = CaseInsensitive
        , optScript  = True
        , optTargets = ["one"]
        }
    assertOutput output
      [ "mv /a/b/one /a/b/one-phat",                   "sync"
      , "mkdir /a/b/one",                              "sync"
      , "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3",   "sync"
      , "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3", "sync"
      , "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3",   "sync"
      , "rmdir /a/b/one-phat",                         "sync"
      ]

------------------------------------------------------------------------------

testFirstNone :: TestTree
testFirstNone = testCase "FirstNone" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat" |->
            Right ["Uno.mp3", "three", "dos.mp3", "two", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one-phat/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one-phat/three" |->
            Right (FileStatus 11 True 3000)
        , expect $ GetFileStatus "/a/b/one-phat/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one-phat/two" |->
            Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b/one-phat/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ RenameFile "/a/b/one-phat/Uno.mp3" "/a/b/one/Uno.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one/three" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat/three" |-> Right []
        , expect $ RemoveDirectory "/a/b/one-phat/three" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one/two" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat/two" |-> Right []
        , expect $ RemoveDirectory "/a/b/one-phat/two" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        ]
      Error.run $ run defaultOptions
        { optTargets = ["one"]
        }
    assertOutput output []

testFirstNoneScript :: TestTree
testFirstNoneScript = testCase "FirstNoneScript" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ ListDirectory "/a/b/one" |->
            Right ["Uno.mp3", "three", "dos.mp3", "two", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one/three" |->
            Right (FileStatus 11 True 3000)
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one/two" |->
            Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ ListDirectory "/a/b/one/three" |-> Right []
        , expect $ ListDirectory "/a/b/one/two" |-> Right []
        ]
      Error.run $ run defaultOptions
        { optScript  = True
        , optTargets = ["one"]
        }
    assertOutput output
      [ "mv /a/b/one /a/b/one-phat",                   "sync"
      , "mkdir /a/b/one",                              "sync"
      , "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3",   "sync"
      , "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3",   "sync"
      , "mkdir /a/b/one/three",                        "sync"
      , "rmdir /a/b/one-phat/three",                   "sync"
      , "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3", "sync"
      , "mkdir /a/b/one/two",                          "sync"
      , "rmdir /a/b/one-phat/two",                     "sync"
      , "rmdir /a/b/one-phat",                         "sync"
      ]

------------------------------------------------------------------------------

testFirstDirs :: TestTree
testFirstDirs = testCase "FirstDirs" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat" |->
            Right ["Uno.mp3", "three", "dos.mp3", "two", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one-phat/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one-phat/three" |->
            Right (FileStatus 11 True 3000)
        , expect $ GetFileStatus "/a/b/one-phat/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one-phat/two" |->
            Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b/one-phat/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ CreateDirectory "/a/b/one/three" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat/three" |-> Right []
        , expect $ RemoveDirectory "/a/b/one-phat/three" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one/two" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat/two" |-> Right []
        , expect $ RemoveDirectory "/a/b/one-phat/two" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/Uno.mp3" "/a/b/one/Uno.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        ]
      Error.run $ run defaultOptions
        { optFirst   = FirstDirs
        , optTargets = ["one"]
        }
    assertOutput output []

testFirstDirsScript :: TestTree
testFirstDirsScript = testCase "FirstDirsScript" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ ListDirectory "/a/b/one" |->
            Right ["Uno.mp3", "three", "dos.mp3", "two", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one/three" |->
            Right (FileStatus 11 True 3000)
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one/two" |->
            Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ ListDirectory "/a/b/one/three" |-> Right []
        , expect $ ListDirectory "/a/b/one/two" |-> Right []
        ]
      Error.run $ run defaultOptions
        { optFirst   = FirstDirs
        , optScript  = True
        , optTargets = ["one"]
        }
    assertOutput output
      [ "mv /a/b/one /a/b/one-phat",                   "sync"
      , "mkdir /a/b/one",                              "sync"
      , "mkdir /a/b/one/three",                        "sync"
      , "rmdir /a/b/one-phat/three",                   "sync"
      , "mkdir /a/b/one/two",                          "sync"
      , "rmdir /a/b/one-phat/two",                     "sync"
      , "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3",   "sync"
      , "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3",   "sync"
      , "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3", "sync"
      , "rmdir /a/b/one-phat",                         "sync"
      ]

------------------------------------------------------------------------------

testFirstFiles :: TestTree
testFirstFiles = testCase "FirstFiles" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat" |->
            Right ["Uno.mp3", "three", "dos.mp3", "two", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one-phat/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one-phat/three" |->
            Right (FileStatus 11 True 3000)
        , expect $ GetFileStatus "/a/b/one-phat/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one-phat/two" |->
            Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b/one-phat/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ RenameFile "/a/b/one-phat/Uno.mp3" "/a/b/one/Uno.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one/three" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat/three" |-> Right []
        , expect $ RemoveDirectory "/a/b/one-phat/three" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one/two" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat/two" |-> Right []
        , expect $ RemoveDirectory "/a/b/one-phat/two" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        ]
      Error.run $ run defaultOptions
        { optFirst   = FirstFiles
        , optTargets = ["one"]
        }
    assertOutput output []

testFirstFilesScript :: TestTree
testFirstFilesScript = testCase "FirstFilesScript" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ ListDirectory "/a/b/one" |->
            Right ["Uno.mp3", "three", "dos.mp3", "two", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one/three" |->
            Right (FileStatus 11 True 3000)
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one/two" |->
            Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ ListDirectory "/a/b/one/three" |-> Right []
        , expect $ ListDirectory "/a/b/one/two" |-> Right []
        ]
      Error.run $ run defaultOptions
        { optFirst   = FirstFiles
        , optScript  = True
        , optTargets = ["one"]
        }
    assertOutput output
      [ "mv /a/b/one /a/b/one-phat",                   "sync"
      , "mkdir /a/b/one",                              "sync"
      , "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3",   "sync"
      , "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3",   "sync"
      , "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3", "sync"
      , "mkdir /a/b/one/three",                        "sync"
      , "rmdir /a/b/one-phat/three",                   "sync"
      , "mkdir /a/b/one/two",                          "sync"
      , "rmdir /a/b/one-phat/two",                     "sync"
      , "rmdir /a/b/one-phat",                         "sync"
      ]

------------------------------------------------------------------------------

testNoSync :: TestTree
testNoSync = testCase "NoSync" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
        , expect $ CreateDirectory "/a/b/one" |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat" |->
            Right ["Uno.mp3", "dos.mp3", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one-phat/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one-phat/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one-phat/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ RenameFile "/a/b/one-phat/Uno.mp3" "/a/b/one/Uno.mp3" |->
            Right ()
        , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
            Right ()
        , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
            Right ()
        , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
        ]
      Error.run $ run defaultOptions
        { optSync    = False
        , optTargets = ["one"]
        }
    assertOutput output []

testNoSyncScript :: TestTree
testNoSyncScript = testCase "NoSyncScript" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ ListDirectory "/a/b/one" |->
            Right ["Uno.mp3", "dos.mp3", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        ]
      Error.run $ run defaultOptions
        { optSync    = False
        , optScript  = True
        , optTargets = ["one"]
        }
    assertOutput output
      [ "mv /a/b/one /a/b/one-phat"
      , "mkdir /a/b/one"
      , "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3"
      , "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3"
      , "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3"
      , "rmdir /a/b/one-phat"
      ]

------------------------------------------------------------------------------

testOrderNameReverse :: TestTree
testOrderNameReverse = testCase "OrderNameReverse" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat" |->
            Right ["Uno.mp3", "three", "dos.mp3", "two", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one-phat/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one-phat/three" |->
            Right (FileStatus 11 True 3000)
        , expect $ GetFileStatus "/a/b/one-phat/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one-phat/two" |->
            Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b/one-phat/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ CreateDirectory "/a/b/one/two" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat/two" |-> Right []
        , expect $ RemoveDirectory "/a/b/one-phat/two" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one/three" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat/three" |-> Right []
        , expect $ RemoveDirectory "/a/b/one-phat/three" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/Uno.mp3" "/a/b/one/Uno.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        ]
      Error.run $ run defaultOptions
        { optFirst   = FirstDirs
        , optReverse = True
        , optTargets = ["one"]
        }
    assertOutput output []

testOrderNameReverseScript :: TestTree
testOrderNameReverseScript = testCase "OrderNameReverseScript" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ ListDirectory "/a/b/one" |->
            Right ["Uno.mp3", "three", "dos.mp3", "two", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one/three" |->
            Right (FileStatus 11 True 3000)
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one/two" |->
            Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ ListDirectory "/a/b/one/two" |-> Right []
        , expect $ ListDirectory "/a/b/one/three" |-> Right []
        ]
      Error.run $ run defaultOptions
        { optFirst   = FirstDirs
        , optReverse = True
        , optScript  = True
        , optTargets = ["one"]
        }
    assertOutput output
      [ "mv /a/b/one /a/b/one-phat",                   "sync"
      , "mkdir /a/b/one",                              "sync"
      , "mkdir /a/b/one/two",                          "sync"
      , "rmdir /a/b/one-phat/two",                     "sync"
      , "mkdir /a/b/one/three",                        "sync"
      , "rmdir /a/b/one-phat/three",                   "sync"
      , "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3", "sync"
      , "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3",   "sync"
      , "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3",   "sync"
      , "rmdir /a/b/one-phat",                         "sync"
      ]

------------------------------------------------------------------------------

testOrderTime :: TestTree
testOrderTime = testCase "OrderTime" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat" |->
            Right ["Uno.mp3", "three", "dos.mp3", "two", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one-phat/Uno.mp3" |->
            Right (FileStatus 11 False 10000)
        , expect $ GetFileStatus "/a/b/one-phat/three" |->
            Right (FileStatus 11 True 3000)
        , expect $ GetFileStatus "/a/b/one-phat/dos.mp3" |->
            Right (FileStatus 11 False 20000)
        , expect $ GetFileStatus "/a/b/one-phat/two" |->
            Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b/one-phat/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ CreateDirectory "/a/b/one/two" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat/two" |-> Right []
        , expect $ RemoveDirectory "/a/b/one-phat/two" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one/three" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat/three" |-> Right []
        , expect $ RemoveDirectory "/a/b/one-phat/three" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/Uno.mp3" "/a/b/one/Uno.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        ]
      Error.run $ run defaultOptions
        { optFirst   = FirstDirs
        , optOrder   = OrderTime
        , optTargets = ["one"]
        }
    assertOutput output []

testOrderTimeScript :: TestTree
testOrderTimeScript = testCase "OrderTimeScript" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ ListDirectory "/a/b/one" |->
            Right ["Uno.mp3", "three", "dos.mp3", "two", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 10000)
        , expect $ GetFileStatus "/a/b/one/three" |->
            Right (FileStatus 11 True 3000)
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 20000)
        , expect $ GetFileStatus "/a/b/one/two" |->
            Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ ListDirectory "/a/b/one/two" |-> Right []
        , expect $ ListDirectory "/a/b/one/three" |-> Right []
        ]
      Error.run $ run defaultOptions
        { optFirst   = FirstDirs
        , optOrder   = OrderTime
        , optScript  = True
        , optTargets = ["one"]
        }
    assertOutput output
      [ "mv /a/b/one /a/b/one-phat",                   "sync"
      , "mkdir /a/b/one",                              "sync"
      , "mkdir /a/b/one/two",                          "sync"
      , "rmdir /a/b/one-phat/two",                     "sync"
      , "mkdir /a/b/one/three",                        "sync"
      , "rmdir /a/b/one-phat/three",                   "sync"
      , "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3", "sync"
      , "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3",   "sync"
      , "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3",   "sync"
      , "rmdir /a/b/one-phat",                         "sync"
      ]

------------------------------------------------------------------------------

testOrderTimeReverse :: TestTree
testOrderTimeReverse = testCase "OrderTimeReverse" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat" |->
            Right ["Uno.mp3", "three", "dos.mp3", "two", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one-phat/Uno.mp3" |->
            Right (FileStatus 11 False 10000)
        , expect $ GetFileStatus "/a/b/one-phat/three" |->
            Right (FileStatus 11 True 3000)
        , expect $ GetFileStatus "/a/b/one-phat/dos.mp3" |->
            Right (FileStatus 11 False 20000)
        , expect $ GetFileStatus "/a/b/one-phat/two" |->
            Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b/one-phat/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ CreateDirectory "/a/b/one/three" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat/three" |-> Right []
        , expect $ RemoveDirectory "/a/b/one-phat/three" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one/two" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat/two" |-> Right []
        , expect $ RemoveDirectory "/a/b/one-phat/two" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/Uno.mp3" "/a/b/one/Uno.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        ]
      Error.run $ run defaultOptions
        { optFirst   = FirstDirs
        , optOrder   = OrderTime
        , optReverse = True
        , optTargets = ["one"]
        }
    assertOutput output []

testOrderTimeReverseScript :: TestTree
testOrderTimeReverseScript = testCase "OrderTimeReverseScript" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ ListDirectory "/a/b/one" |->
            Right ["Uno.mp3", "three", "dos.mp3", "two", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 10000)
        , expect $ GetFileStatus "/a/b/one/three" |->
            Right (FileStatus 11 True 3000)
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 20000)
        , expect $ GetFileStatus "/a/b/one/two" |->
            Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ ListDirectory "/a/b/one/three" |-> Right []
        , expect $ ListDirectory "/a/b/one/two" |-> Right []
        ]
      Error.run $ run defaultOptions
        { optFirst   = FirstDirs
        , optOrder   = OrderTime
        , optReverse = True
        , optScript  = True
        , optTargets = ["one"]
        }
    assertOutput output
      [ "mv /a/b/one /a/b/one-phat",                   "sync"
      , "mkdir /a/b/one",                              "sync"
      , "mkdir /a/b/one/three",                        "sync"
      , "rmdir /a/b/one-phat/three",                   "sync"
      , "mkdir /a/b/one/two",                          "sync"
      , "rmdir /a/b/one-phat/two",                     "sync"
      , "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3",   "sync"
      , "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3",   "sync"
      , "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3", "sync"
      , "rmdir /a/b/one-phat",                         "sync"
      ]

------------------------------------------------------------------------------

-- NOTE: Random order not consistent across multiple library versions
testOrderRandom :: TestTree
testOrderRandom = testCase "OrderRandom" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat" |->
            Right ["x.mp3", "y.mp3", "z.mp3", "d1", "d2", "d3"]
        , expect $ GetFileStatus "/a/b/one-phat/x.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one-phat/y.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one-phat/z.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ GetFileStatus "/a/b/one-phat/d1" |->
            Right (FileStatus 11 True 1000)
        , expect $ GetFileStatus "/a/b/one-phat/d2" |->
            Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b/one-phat/d3" |->
            Right (FileStatus 11 True 3000)
        , inAnyOrder
            [ inSequence
                [ expect $ CreateDirectory "/a/b/one/d1" |-> Right ()
                , expect $ CallProcess "sync" [] |-> Right ()
                , expect $ ListDirectory "/a/b/one-phat/d1" |-> Right []
                , expect $ RemoveDirectory "/a/b/one-phat/d1" |-> Right ()
                , expect $ CallProcess "sync" [] |-> Right ()
                ]
            , inSequence
                [ expect $ CreateDirectory "/a/b/one/d2" |-> Right ()
                , expect $ CallProcess "sync" [] |-> Right ()
                , expect $ ListDirectory "/a/b/one-phat/d2" |-> Right []
                , expect $ RemoveDirectory "/a/b/one-phat/d2" |-> Right ()
                , expect $ CallProcess "sync" [] |-> Right ()
                ]
            , inSequence
                [ expect $ CreateDirectory "/a/b/one/d3" |-> Right ()
                , expect $ CallProcess "sync" [] |-> Right ()
                , expect $ ListDirectory "/a/b/one-phat/d3" |-> Right []
                , expect $ RemoveDirectory "/a/b/one-phat/d3" |-> Right ()
                , expect $ CallProcess "sync" [] |-> Right ()
                ]
            ]
        , inAnyOrder
            [ inSequence
                [ expect $
                    RenameFile "/a/b/one-phat/x.mp3" "/a/b/one/x.mp3" |->
                      Right ()
                , expect $ CallProcess "sync" [] |-> Right ()
                ]
            , inSequence
                [ expect $
                    RenameFile "/a/b/one-phat/y.mp3" "/a/b/one/y.mp3" |->
                      Right ()
                , expect $ CallProcess "sync" [] |-> Right ()
                ]
            , inSequence
                [ expect $
                    RenameFile "/a/b/one-phat/z.mp3" "/a/b/one/z.mp3" |->
                      Right ()
                , expect $ CallProcess "sync" [] |-> Right ()
                ]
            ]
        , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        ]
      Error.run $ run defaultOptions
        { optFirst   = FirstDirs
        , optOrder   = OrderRandom
        , optTargets = ["one"]
        }
    assertOutput output []

-- TODO FIX broken across different version of random
testOrderRandomScript :: TestTree
testOrderRandomScript = testCase "OrderRandomScript" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ ListDirectory "/a/b/one" |->
            Right ["x.mp3", "y.mp3", "z.mp3", "d1", "d2", "d3"]
        , expect $ GetFileStatus "/a/b/one/x.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one/y.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one/z.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ GetFileStatus "/a/b/one/d1" |->
            Right (FileStatus 11 True 1000)
        , expect $ GetFileStatus "/a/b/one/d2" |->
            Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b/one/d3" |->
            Right (FileStatus 11 True 3000)
        , expect $ ListDirectory "/a/b/one/d1" |-> Right []
        , expect $ ListDirectory "/a/b/one/d3" |-> Right []
        , expect $ ListDirectory "/a/b/one/d2" |-> Right []
        ]
      Error.run $ run defaultOptions
        { optFirst   = FirstDirs
        , optOrder   = OrderRandom
        , optScript  = True
        , optTargets = ["one"]
        }
    assertOutput output
      [ "mv /a/b/one /a/b/one-phat",             "sync"
      , "mkdir /a/b/one",                        "sync"
      , "mkdir /a/b/one/d1",                     "sync"
      , "rmdir /a/b/one-phat/d1",                "sync"
      , "mkdir /a/b/one/d3",                     "sync"
      , "rmdir /a/b/one-phat/d3",                "sync"
      , "mkdir /a/b/one/d2",                     "sync"
      , "rmdir /a/b/one-phat/d2",                "sync"
      , "mv /a/b/one-phat/y.mp3 /a/b/one/y.mp3", "sync"
      , "mv /a/b/one-phat/x.mp3 /a/b/one/x.mp3", "sync"
      , "mv /a/b/one-phat/z.mp3 /a/b/one/z.mp3", "sync"
      , "rmdir /a/b/one-phat",                   "sync"
      ]

------------------------------------------------------------------------------

testVerbose :: TestTree
testVerbose = testCase "Verbose" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat" |->
            Right ["Uno.mp3", "dos.mp3", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one-phat/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one-phat/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one-phat/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ RenameFile "/a/b/one-phat/Uno.mp3" "/a/b/one/Uno.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        ]
      Error.run $ run defaultOptions
        { optVerbose = True
        , optTargets = ["one"]
        }
    assertOutput output
      [ "one"
      , "one/Uno.mp3"
      , "one/dos.mp3"
      , "one/tres.mp3"
      ]

testVerboseScript :: TestTree
testVerboseScript = testCase "VerboseScript" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ ListDirectory "/a/b/one" |->
            Right ["Uno.mp3", "dos.mp3", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        ]
      Error.run $ run defaultOptions
        { optScript  = True
        , optVerbose = True
        , optTargets = ["one"]
        }
    assertOutput output
      [ "echo one"
      , "mv /a/b/one /a/b/one-phat",                   "sync"
      , "mkdir /a/b/one",                              "sync"
      , "echo one/Uno.mp3"
      , "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3",   "sync"
      , "echo one/dos.mp3"
      , "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3",   "sync"
      , "echo one/tres.mp3"
      , "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3", "sync"
      , "rmdir /a/b/one-phat",                         "sync"
      ]

------------------------------------------------------------------------------

testMultipleTargets :: TestTree
testMultipleTargets = testCase "MultipleTargets" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ MakeAbsolute "two" |-> Right "/a/b/two"
        , expect $ GetFileStatus "/a/b/two" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/two-phat" |-> Right False
        , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/one" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/one-phat" |->
            Right ["Uno.mp3", "dos.mp3", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one-phat/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one-phat/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one-phat/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ RenameFile "/a/b/one-phat/Uno.mp3" "/a/b/one/Uno.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameDirectory "/a/b/two" "/a/b/two-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/two" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/two-phat" |->
            Right ["san.mp3", "ni.mp3", "ichi.mp3"]
        , expect $ GetFileStatus "/a/b/two-phat/san.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ GetFileStatus "/a/b/two-phat/ni.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/two-phat/ichi.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ RenameFile "/a/b/two-phat/ichi.mp3" "/a/b/two/ichi.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/two-phat/ni.mp3" "/a/b/two/ni.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/two-phat/san.mp3" "/a/b/two/san.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/two-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        ]
      Error.run $ run defaultOptions
        { optTargets = ["one", "two"]
        }
    assertOutput output []

testMultipleTargetsScript :: TestTree
testMultipleTargetsScript = testCase "MultipleTargetsScript" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
        , expect $ MakeAbsolute "two" |-> Right "/a/b/two"
        , expect $ GetFileStatus "/a/b/two" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/two-phat" |-> Right False
        , expect $ ListDirectory "/a/b/one" |->
            Right ["Uno.mp3", "dos.mp3", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ ListDirectory "/a/b/two" |->
            Right ["san.mp3", "ni.mp3", "ichi.mp3"]
        , expect $ GetFileStatus "/a/b/two/san.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ GetFileStatus "/a/b/two/ni.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/two/ichi.mp3" |->
            Right (FileStatus 11 False 1000)
        ]
      Error.run $ run defaultOptions
        { optScript  = True
        , optTargets = ["one", "two"]
        }
    assertOutput output
      [ "mv /a/b/one /a/b/one-phat",                   "sync"
      , "mkdir /a/b/one",                              "sync"
      , "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3",   "sync"
      , "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3",   "sync"
      , "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3", "sync"
      , "rmdir /a/b/one-phat",                         "sync"
      , "mv /a/b/two /a/b/two-phat",                   "sync"
      , "mkdir /a/b/two",                              "sync"
      , "mv /a/b/two-phat/ichi.mp3 /a/b/two/ichi.mp3", "sync"
      , "mv /a/b/two-phat/ni.mp3 /a/b/two/ni.mp3",     "sync"
      , "mv /a/b/two-phat/san.mp3 /a/b/two/san.mp3",   "sync"
      , "rmdir /a/b/two-phat",                         "sync"
      ]

------------------------------------------------------------------------------

testTargetNotFound :: TestTree
testTargetNotFound = testCase "TargetNotFound" $ do
    Left err <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |->
            Left (userError "file not found: one")
        ]
      Error.run $ run defaultOptions
        { optTargets = ["one"]
        }
    err @=? "user error (file not found: one)"

------------------------------------------------------------------------------

testPhatTarget :: TestTree
testPhatTarget = testCase "PhatTarget" $ do
    Left err <- runTest . Error.run $ run defaultOptions
        { optTargets = ["one-phat"]
        }
    err @=? "-phat directory: one-phat"

------------------------------------------------------------------------------

testTargetNotDirectory :: TestTree
testTargetNotDirectory = testCase "TargetNotDirectory" $ do
    Left err <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 False 10000)
        ]
      Error.run $ run defaultOptions
        { optTargets = ["one"]
        }
    err @=? "not a directory: one"

------------------------------------------------------------------------------

testTargetMountPoint :: TestTree
testTargetMountPoint = testCase "TargetMountPoint" $ do
    Left err <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/one"
        , expect $ GetFileStatus "/a/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a" |-> Right (FileStatus 10 True 100)
        ]
      Error.run $ run defaultOptions
        { optTargets = ["one"]
        }
    err @=? "mount point: one"

------------------------------------------------------------------------------

testTargetPhatExists :: TestTree
testTargetPhatExists = testCase "TargetPhatExists" $ do
    Left err <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/one-phat" |-> Right True
        ]
      Error.run $ run defaultOptions
        { optTargets = ["one"]
        }
    err @=? "already exists: one-phat"

------------------------------------------------------------------------------

testLarge :: TestTree
testLarge = testCase "Large" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "c" |-> Right "/a/b/c"
        , expect $ GetFileStatus "/a/b/c" |-> Right (FileStatus 11 True 1000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/c-phat" |-> Right False
        , expect $ MakeAbsolute "d" |-> Right "/a/b/d"
        , expect $ GetFileStatus "/a/b/d" |-> Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/d-phat" |-> Right False
        , expect $ RenameDirectory "/a/b/c" "/a/b/c-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/c" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/c-phat" |-> Right ["two", "one"]
        , expect $ GetFileStatus "/a/b/c-phat/two" |->
            Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b/c-phat/one" |->
            Right (FileStatus 11 True 1000)
        , expect $ CreateDirectory "/a/b/c/one" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/c-phat/one" |->
            Right ["11.mp3", "six", "13.mp3", "eight", "12.mp3", "14.mp3"]
        , expect $ GetFileStatus "/a/b/c-phat/one/11.mp3" |->
            Right (FileStatus 11 False 1100)
        , expect $ GetFileStatus "/a/b/c-phat/one/six" |->
            Right (FileStatus 11 True 6000)
        , expect $ GetFileStatus "/a/b/c-phat/one/13.mp3" |->
            Right (FileStatus 11 False 1300)
        , expect $ GetFileStatus "/a/b/c-phat/one/eight" |->
            Right (FileStatus 11 True 8000)
        , expect $ GetFileStatus "/a/b/c-phat/one/12.mp3" |->
            Right (FileStatus 11 False 1200)
        , expect $ GetFileStatus "/a/b/c-phat/one/14.mp3" |->
            Right (FileStatus 11 False 1400)
        , expect $ RenameFile "/a/b/c-phat/one/11.mp3" "/a/b/c/one/11.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/c-phat/one/12.mp3" "/a/b/c/one/12.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/c-phat/one/13.mp3" "/a/b/c/one/13.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/c-phat/one/14.mp3" "/a/b/c/one/14.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/c/one/eight" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/c-phat/one/eight" |->
            Right ["83.mp3", "81.mp3", "82.mp3"]
        , expect $ GetFileStatus "/a/b/c-phat/one/eight/83.mp3" |->
            Right (FileStatus 11 False 8300)
        , expect $ GetFileStatus "/a/b/c-phat/one/eight/81.mp3" |->
            Right (FileStatus 11 False 8100)
        , expect $ GetFileStatus "/a/b/c-phat/one/eight/82.mp3" |->
            Right (FileStatus 11 False 8200)
        , expect $
            RenameFile
              "/a/b/c-phat/one/eight/81.mp3"
              "/a/b/c/one/eight/81.mp3"
              |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $
            RenameFile
              "/a/b/c-phat/one/eight/82.mp3"
              "/a/b/c/one/eight/82.mp3"
              |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $
            RenameFile
              "/a/b/c-phat/one/eight/83.mp3"
              "/a/b/c/one/eight/83.mp3"
              |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/c-phat/one/eight" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/c/one/six" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/c-phat/one/six" |-> Right []
        , expect $ RemoveDirectory "/a/b/c-phat/one/six" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/c-phat/one" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/c/two" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/c-phat/two" |->
            Right ["23.mp3", "22.mp3", "seven", "five", "21.mp3"]
        , expect $ GetFileStatus "/a/b/c-phat/two/23.mp3" |->
            Right (FileStatus 11 False 2300)
        , expect $ GetFileStatus "/a/b/c-phat/two/22.mp3" |->
            Right (FileStatus 11 False 2200)
        , expect $ GetFileStatus "/a/b/c-phat/two/seven" |->
            Right (FileStatus 11 True 7000)
        , expect $ GetFileStatus "/a/b/c-phat/two/five" |->
            Right (FileStatus 11 True 5000)
        , expect $ GetFileStatus "/a/b/c-phat/two/21.mp3" |->
            Right (FileStatus 11 False 2100)
        , expect $ RenameFile "/a/b/c-phat/two/21.mp3" "/a/b/c/two/21.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/c-phat/two/22.mp3" "/a/b/c/two/22.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameFile "/a/b/c-phat/two/23.mp3" "/a/b/c/two/23.mp3" |->
            Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/c/two/five" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/c-phat/two/five" |-> Right ["51.mp3"]
        , expect $ GetFileStatus "/a/b/c-phat/two/five/51.mp3" |->
            Right (FileStatus 11 False 5100)
        , expect $
            RenameFile "/a/b/c-phat/two/five/51.mp3" "/a/b/c/two/five/51.mp3"
            |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/c-phat/two/five" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/c/two/seven" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/c-phat/two/seven" |->
            Right ["72.mp3", "71.mp3"]
        , expect $ GetFileStatus "/a/b/c-phat/two/seven/72.mp3" |->
            Right (FileStatus 11 False 7200)
        , expect $ GetFileStatus "/a/b/c-phat/two/seven/71.mp3" |->
            Right (FileStatus 11 False 7100)
        , expect $
            RenameFile
              "/a/b/c-phat/two/seven/71.mp3"
              "/a/b/c/two/seven/71.mp3"
              |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $
            RenameFile
              "/a/b/c-phat/two/seven/72.mp3"
              "/a/b/c/two/seven/72.mp3"
              |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/c-phat/two/seven" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/c-phat/two" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/c-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RenameDirectory "/a/b/d" "/a/b/d-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/d" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/d-phat" |-> Right ["three"]
        , expect $ GetFileStatus "/a/b/d-phat/three" |->
            Right (FileStatus 11 True 3000)
        , expect $ CreateDirectory "/a/b/d/three" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/d-phat/three" |->
            Right ["32.mp3", "31.mp3", "four", "34.mp3", "nine", "33.mp3"]
        , expect $ GetFileStatus "/a/b/d-phat/three/32.mp3" |->
            Right (FileStatus 11 False 3200)
        , expect $ GetFileStatus "/a/b/d-phat/three/31.mp3" |->
            Right (FileStatus 11 False 3100)
        , expect $ GetFileStatus "/a/b/d-phat/three/four" |->
            Right (FileStatus 11 True 4000)
        , expect $ GetFileStatus "/a/b/d-phat/three/34.mp3" |->
            Right (FileStatus 11 False 3400)
        , expect $ GetFileStatus "/a/b/d-phat/three/nine" |->
            Right (FileStatus 11 True 9000)
        , expect $ GetFileStatus "/a/b/d-phat/three/33.mp3" |->
            Right (FileStatus 11 False 3300)
        , expect $
            RenameFile "/a/b/d-phat/three/31.mp3" "/a/b/d/three/31.mp3" |->
              Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $
            RenameFile "/a/b/d-phat/three/32.mp3" "/a/b/d/three/32.mp3" |->
              Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $
            RenameFile "/a/b/d-phat/three/33.mp3" "/a/b/d/three/33.mp3" |->
              Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $
            RenameFile "/a/b/d-phat/three/34.mp3" "/a/b/d/three/34.mp3" |->
              Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/d/three/four" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/d-phat/three/four" |->
            Right ["41.mp3", "42.mp3"]
        , expect $ GetFileStatus "/a/b/d-phat/three/four/41.mp3" |->
            Right (FileStatus 11 False 4100)
        , expect $ GetFileStatus "/a/b/d-phat/three/four/42.mp3" |->
            Right (FileStatus 11 False 4200)
        , expect $
            RenameFile
              "/a/b/d-phat/three/four/41.mp3"
              "/a/b/d/three/four/41.mp3"
            |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $
            RenameFile
              "/a/b/d-phat/three/four/42.mp3"
              "/a/b/d/three/four/42.mp3"
            |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/d-phat/three/four" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ CreateDirectory "/a/b/d/three/nine" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/d-phat/three/nine" |-> Right ["ten"]
        , expect $ GetFileStatus "/a/b/d-phat/three/nine/ten" |->
            Right (FileStatus 11 True 10000)
        , expect $ CreateDirectory "/a/b/d/three/nine/ten" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ ListDirectory "/a/b/d-phat/three/nine/ten" |->
            Right ["102.mp3", "101.mp3"]
        , expect $ GetFileStatus "/a/b/d-phat/three/nine/ten/102.mp3" |->
            Right (FileStatus 11 False 10200)
        , expect $ GetFileStatus "/a/b/d-phat/three/nine/ten/101.mp3" |->
            Right (FileStatus 11 False 10100)
        , expect $
            RenameFile
              "/a/b/d-phat/three/nine/ten/101.mp3"
              "/a/b/d/three/nine/ten/101.mp3"
              |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $
            RenameFile
              "/a/b/d-phat/three/nine/ten/102.mp3"
              "/a/b/d/three/nine/ten/102.mp3"
              |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/d-phat/three/nine/ten" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/d-phat/three/nine" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/d-phat/three" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        , expect $ RemoveDirectory "/a/b/d-phat" |-> Right ()
        , expect $ CallProcess "sync" [] |-> Right ()
        ]
      Error.run $ run defaultOptions
        { optTargets = ["c", "d"]
        }
    assertOutput output []

testLargeScript :: TestTree
testLargeScript = testCase "LargeScript" $ do
    Right output <- runTest $ do
      inSequence
        [ expect $ MakeAbsolute "c" |-> Right "/a/b/c"
        , expect $ GetFileStatus "/a/b/c" |-> Right (FileStatus 11 True 1000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/c-phat" |-> Right False
        , expect $ MakeAbsolute "d" |-> Right "/a/b/d"
        , expect $ GetFileStatus "/a/b/d" |-> Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
        , expect $ DoesPathExist "/a/b/d-phat" |-> Right False
        , expect $ ListDirectory "/a/b/c" |-> Right ["two", "one"]
        , expect $ GetFileStatus "/a/b/c/two" |->
            Right (FileStatus 11 True 2000)
        , expect $ GetFileStatus "/a/b/c/one" |->
            Right (FileStatus 11 True 1000)
        , expect $ ListDirectory "/a/b/c/one" |->
            Right ["11.mp3", "six", "13.mp3", "eight", "12.mp3", "14.mp3"]
        , expect $ GetFileStatus "/a/b/c/one/11.mp3" |->
            Right (FileStatus 11 False 1100)
        , expect $ GetFileStatus "/a/b/c/one/six" |->
            Right (FileStatus 11 True 6000)
        , expect $ GetFileStatus "/a/b/c/one/13.mp3" |->
            Right (FileStatus 11 False 1300)
        , expect $ GetFileStatus "/a/b/c/one/eight" |->
            Right (FileStatus 11 True 8000)
        , expect $ GetFileStatus "/a/b/c/one/12.mp3" |->
            Right (FileStatus 11 False 1200)
        , expect $ GetFileStatus "/a/b/c/one/14.mp3" |->
            Right (FileStatus 11 False 1400)
        , expect $ ListDirectory "/a/b/c/one/eight" |->
            Right ["83.mp3", "81.mp3", "82.mp3"]
        , expect $ GetFileStatus "/a/b/c/one/eight/83.mp3" |->
            Right (FileStatus 11 False 8300)
        , expect $ GetFileStatus "/a/b/c/one/eight/81.mp3" |->
            Right (FileStatus 11 False 8100)
        , expect $ GetFileStatus "/a/b/c/one/eight/82.mp3" |->
            Right (FileStatus 11 False 8200)
        , expect $ ListDirectory "/a/b/c/one/six" |-> Right []
        , expect $ ListDirectory "/a/b/c/two" |->
            Right ["23.mp3", "22.mp3", "seven", "five", "21.mp3"]
        , expect $ GetFileStatus "/a/b/c/two/23.mp3" |->
            Right (FileStatus 11 False 2300)
        , expect $ GetFileStatus "/a/b/c/two/22.mp3" |->
            Right (FileStatus 11 False 2200)
        , expect $ GetFileStatus "/a/b/c/two/seven" |->
            Right (FileStatus 11 True 7000)
        , expect $ GetFileStatus "/a/b/c/two/five" |->
            Right (FileStatus 11 True 5000)
        , expect $ GetFileStatus "/a/b/c/two/21.mp3" |->
            Right (FileStatus 11 False 2100)
        , expect $ ListDirectory "/a/b/c/two/five" |-> Right ["51.mp3"]
        , expect $ GetFileStatus "/a/b/c/two/five/51.mp3" |->
            Right (FileStatus 11 False 5100)
        , expect $ ListDirectory "/a/b/c/two/seven" |->
            Right ["72.mp3", "71.mp3"]
        , expect $ GetFileStatus "/a/b/c/two/seven/72.mp3" |->
            Right (FileStatus 11 False 7200)
        , expect $ GetFileStatus "/a/b/c/two/seven/71.mp3" |->
            Right (FileStatus 11 False 7100)
        , expect $ ListDirectory "/a/b/d" |-> Right ["three"]
        , expect $ GetFileStatus "/a/b/d/three" |->
            Right (FileStatus 11 True 3000)
        , expect $ ListDirectory "/a/b/d/three" |->
            Right ["32.mp3", "31.mp3", "four", "34.mp3", "nine", "33.mp3"]
        , expect $ GetFileStatus "/a/b/d/three/32.mp3" |->
            Right (FileStatus 11 False 3200)
        , expect $ GetFileStatus "/a/b/d/three/31.mp3" |->
            Right (FileStatus 11 False 3100)
        , expect $ GetFileStatus "/a/b/d/three/four" |->
            Right (FileStatus 11 True 4000)
        , expect $ GetFileStatus "/a/b/d/three/34.mp3" |->
            Right (FileStatus 11 False 3400)
        , expect $ GetFileStatus "/a/b/d/three/nine" |->
            Right (FileStatus 11 True 9000)
        , expect $ GetFileStatus "/a/b/d/three/33.mp3" |->
            Right (FileStatus 11 False 3300)
        , expect $ ListDirectory "/a/b/d/three/four" |->
            Right ["41.mp3", "42.mp3"]
        , expect $ GetFileStatus "/a/b/d/three/four/41.mp3" |->
            Right (FileStatus 11 False 4100)
        , expect $ GetFileStatus "/a/b/d/three/four/42.mp3" |->
            Right (FileStatus 11 False 4200)
        , expect $ ListDirectory "/a/b/d/three/nine" |-> Right ["ten"]
        , expect $ GetFileStatus "/a/b/d/three/nine/ten" |->
            Right (FileStatus 11 True 10000)
        , expect $ ListDirectory "/a/b/d/three/nine/ten" |->
            Right ["102.mp3", "101.mp3"]
        , expect $ GetFileStatus "/a/b/d/three/nine/ten/102.mp3" |->
            Right (FileStatus 11 False 10200)
        , expect $ GetFileStatus "/a/b/d/three/nine/ten/101.mp3" |->
            Right (FileStatus 11 False 10100)
        ]
      Error.run $ run defaultOptions
        { optSync    = False
        , optScript  = True
        , optTargets = ["c", "d"]
        }
    assertOutput output
      [ "mv /a/b/c /a/b/c-phat"
      , "mkdir /a/b/c"
      , "mkdir /a/b/c/one"
      , "mv /a/b/c-phat/one/11.mp3 /a/b/c/one/11.mp3"
      , "mv /a/b/c-phat/one/12.mp3 /a/b/c/one/12.mp3"
      , "mv /a/b/c-phat/one/13.mp3 /a/b/c/one/13.mp3"
      , "mv /a/b/c-phat/one/14.mp3 /a/b/c/one/14.mp3"
      , "mkdir /a/b/c/one/eight"
      , "mv /a/b/c-phat/one/eight/81.mp3 /a/b/c/one/eight/81.mp3"
      , "mv /a/b/c-phat/one/eight/82.mp3 /a/b/c/one/eight/82.mp3"
      , "mv /a/b/c-phat/one/eight/83.mp3 /a/b/c/one/eight/83.mp3"
      , "rmdir /a/b/c-phat/one/eight"
      , "mkdir /a/b/c/one/six"
      , "rmdir /a/b/c-phat/one/six"
      , "rmdir /a/b/c-phat/one"
      , "mkdir /a/b/c/two"
      , "mv /a/b/c-phat/two/21.mp3 /a/b/c/two/21.mp3"
      , "mv /a/b/c-phat/two/22.mp3 /a/b/c/two/22.mp3"
      , "mv /a/b/c-phat/two/23.mp3 /a/b/c/two/23.mp3"
      , "mkdir /a/b/c/two/five"
      , "mv /a/b/c-phat/two/five/51.mp3 /a/b/c/two/five/51.mp3"
      , "rmdir /a/b/c-phat/two/five"
      , "mkdir /a/b/c/two/seven"
      , "mv /a/b/c-phat/two/seven/71.mp3 /a/b/c/two/seven/71.mp3"
      , "mv /a/b/c-phat/two/seven/72.mp3 /a/b/c/two/seven/72.mp3"
      , "rmdir /a/b/c-phat/two/seven"
      , "rmdir /a/b/c-phat/two"
      , "rmdir /a/b/c-phat"
      , "mv /a/b/d /a/b/d-phat"
      , "mkdir /a/b/d"
      , "mkdir /a/b/d/three"
      , "mv /a/b/d-phat/three/31.mp3 /a/b/d/three/31.mp3"
      , "mv /a/b/d-phat/three/32.mp3 /a/b/d/three/32.mp3"
      , "mv /a/b/d-phat/three/33.mp3 /a/b/d/three/33.mp3"
      , "mv /a/b/d-phat/three/34.mp3 /a/b/d/three/34.mp3"
      , "mkdir /a/b/d/three/four"
      , "mv /a/b/d-phat/three/four/41.mp3 /a/b/d/three/four/41.mp3"
      , "mv /a/b/d-phat/three/four/42.mp3 /a/b/d/three/four/42.mp3"
      , "rmdir /a/b/d-phat/three/four"
      , "mkdir /a/b/d/three/nine"
      , "mkdir /a/b/d/three/nine/ten"
      , "mv /a/b/d-phat/three/nine/ten/101.mp3 /a/b/d/three/nine/ten/101.mp3"
      , "mv /a/b/d-phat/three/nine/ten/102.mp3 /a/b/d/three/nine/ten/102.mp3"
      , "rmdir /a/b/d-phat/three/nine/ten"
      , "rmdir /a/b/d-phat/three/nine"
      , "rmdir /a/b/d-phat/three"
      , "rmdir /a/b/d-phat"
      ]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "phatsort:HMock"
    [ testCaseSensitive
    , testCaseSensitiveScript
    , testCaseInsensitive
    , testCaseInsensitiveScript
    , testFirstNone
    , testFirstNoneScript
    , testFirstDirs
    , testFirstDirsScript
    , testFirstFiles
    , testFirstFilesScript
    , testNoSync
    , testNoSyncScript
    , testOrderNameReverse
    , testOrderNameReverseScript
    , testOrderTime
    , testOrderTimeScript
    , testOrderTimeReverse
    , testOrderTimeReverseScript
    , testOrderRandom
    , testOrderRandomScript
    , testVerbose
    , testVerboseScript
    , testMultipleTargets
    , testMultipleTargetsScript
    , testTargetNotFound
    , testPhatTarget
    , testTargetNotDirectory
    , testTargetMountPoint
    , testTargetPhatExists
    , testLarge
    , testLargeScript
    ]

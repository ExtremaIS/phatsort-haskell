module PhatSort.Cmd.SeqCp.HMock (tests) where

-- https://hackage.haskell.org/package/base
import Control.Monad ((<=<))
import qualified Data.List.NonEmpty as NonEmpty

-- https://hackage.haskell.org/package/HMock
import Test.HMock ((|->), expect, inAnyOrder, inSequence, runMockT)

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit (testCase)

-- (phatsort)
import PhatSort.Cmd.SeqCp
  ( Options
      ( Options, optCase, optDestination, optFirst, optOrder, optReverse
      , optScript, optSources, optSync, optVerbose
      )
  , run
  )
import PhatSort.Monad.FileSystem (FileStatus(FileStatus))
import qualified PhatSort.Monad.Trans.Error as Error
import PhatSort.SortOptions
  ( SortCase(CaseInsensitive, CaseSensitive)
  , SortFirst(FirstDirs, FirstFiles, FirstNone)
  , SortOrder(OrderName, OrderRandom, OrderTime)
  )

-- (phatsort:test)
import TestLib

------------------------------------------------------------------------------

defaultOptions :: Options
defaultOptions = Options
    { optCase        = CaseSensitive
    , optFirst       = FirstNone
    , optSync        = True
    , optOrder       = OrderName
    , optReverse     = False
    , optScript      = False
    , optVerbose     = False
    , optSources     = NonEmpty.fromList ["one"]
    , optDestination = "/z"
    }

sourcesOptions :: Options
sourcesOptions = defaultOptions
    { optSources =
        NonEmpty.fromList ["python.mp4", "/a/b/two", "one", "c/Haskell.mp4"]
    }

------------------------------------------------------------------------------

testCaseSensitive :: TestTree
testCaseSensitive = testCase "CaseSensitive" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ CreateDirectory "/z/one" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions

------------------------------------------------------------------------------

testCaseSensitiveScript :: TestTree
testCaseSensitiveScript = testCase "CaseSensitiveScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ PutStrLn "mkdir /z/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optScript = True
      }

------------------------------------------------------------------------------

testSourcesCaseSensitive :: TestTree
testSourcesCaseSensitive = testCase "SourcesCaseSensitive" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "python.mp4" |-> Right "/a/b/python.mp4"
      , expect $ GetFileStatus "/a/b/python.mp4" |->
          Right (FileStatus 11 False 30000)
      , expect $ DoesPathExist "/z/python.mp4" |-> Right False
      , expect $ MakeAbsolute "/a/b/two" |-> Right "/a/b/two"
      , expect $ GetFileStatus "/a/b/two" |-> Right (FileStatus 11 True 20000)
      , expect $ DoesPathExist "/z/two" |-> Right False
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ MakeAbsolute "c/Haskell.mp4" |-> Right "/a/b/c/Haskell.mp4"
      , expect $ GetFileStatus "/a/b/c/Haskell.mp4" |->
          Right (FileStatus 11 False 40000)
      , expect $ DoesPathExist "/z/Haskell.mp4" |-> Right False
      , expect $ CopyFile "/a/b/c/Haskell.mp4" "/z/Haskell.mp4" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/z/one" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/python.mp4" "/z/python.mp4" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/z/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/two" |-> Right []
      ]
    assertSuccess <=< Error.run $ run sourcesOptions

------------------------------------------------------------------------------

testSourcesCaseSensitiveScript :: TestTree
testSourcesCaseSensitiveScript =
    testCase "SourcesCaseSensitiveScript" . runMockT $ do
      inSequence
        [ expect $ MakeAbsolute "/z" |-> Right "/z"
        , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
        , expect $ MakeAbsolute "python.mp4" |-> Right "/a/b/python.mp4"
        , expect $ GetFileStatus "/a/b/python.mp4" |->
            Right (FileStatus 11 False 30000)
        , expect $ DoesPathExist "/z/python.mp4" |-> Right False
        , expect $ MakeAbsolute "/a/b/two" |-> Right "/a/b/two"
        , expect $ GetFileStatus "/a/b/two" |->
            Right (FileStatus 11 True 20000)
        , expect $ DoesPathExist "/z/two" |-> Right False
        , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ DoesPathExist "/z/one" |-> Right False
        , expect $ MakeAbsolute "c/Haskell.mp4" |-> Right "/a/b/c/Haskell.mp4"
        , expect $ GetFileStatus "/a/b/c/Haskell.mp4" |->
            Right (FileStatus 11 False 40000)
        , expect $ DoesPathExist "/z/Haskell.mp4" |-> Right False
        , expect $ PutStrLn "cp /a/b/c/Haskell.mp4 /z/Haskell.mp4" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "mkdir /z/one" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ ListDirectory "/a/b/one" |->
            Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "cp /a/b/python.mp4 /z/python.mp4" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "mkdir /z/two" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ ListDirectory "/a/b/two" |-> Right []
        ]
      assertSuccess <=< Error.run $ run sourcesOptions
        { optScript = True
        }

------------------------------------------------------------------------------

testCaseInsensitive :: TestTree
testCaseInsensitive = testCase "CaseInsensitive" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ CreateDirectory "/z/one" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optCase = CaseInsensitive
      }

------------------------------------------------------------------------------

testCaseInsensitiveScript :: TestTree
testCaseInsensitiveScript = testCase "CaseInsensitiveScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ PutStrLn "mkdir /z/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optCase = CaseInsensitive
      , optScript = True
      }

------------------------------------------------------------------------------

testSourcesCaseInsensitive :: TestTree
testSourcesCaseInsensitive = testCase "SourcesCaseInsensitive" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "python.mp4" |-> Right "/a/b/python.mp4"
      , expect $ GetFileStatus "/a/b/python.mp4" |->
          Right (FileStatus 11 False 30000)
      , expect $ DoesPathExist "/z/python.mp4" |-> Right False
      , expect $ MakeAbsolute "/a/b/two" |-> Right "/a/b/two"
      , expect $ GetFileStatus "/a/b/two" |-> Right (FileStatus 11 True 20000)
      , expect $ DoesPathExist "/z/two" |-> Right False
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ MakeAbsolute "c/Haskell.mp4" |-> Right "/a/b/c/Haskell.mp4"
      , expect $ GetFileStatus "/a/b/c/Haskell.mp4" |->
          Right (FileStatus 11 False 40000)
      , expect $ DoesPathExist "/z/Haskell.mp4" |-> Right False
      , expect $ CopyFile "/a/b/c/Haskell.mp4" "/z/Haskell.mp4" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/z/one" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/python.mp4" "/z/python.mp4" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/z/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/two" |-> Right []
      ]
    assertSuccess <=< Error.run $ run sourcesOptions
      { optCase = CaseInsensitive
      }

------------------------------------------------------------------------------

testSourcesCaseInsensitiveScript :: TestTree
testSourcesCaseInsensitiveScript =
    testCase "SourcesCaseInsensitiveScript" . runMockT $ do
      inSequence
        [ expect $ MakeAbsolute "/z" |-> Right "/z"
        , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
        , expect $ MakeAbsolute "python.mp4" |-> Right "/a/b/python.mp4"
        , expect $ GetFileStatus "/a/b/python.mp4" |->
            Right (FileStatus 11 False 30000)
        , expect $ DoesPathExist "/z/python.mp4" |-> Right False
        , expect $ MakeAbsolute "/a/b/two" |-> Right "/a/b/two"
        , expect $ GetFileStatus "/a/b/two" |->
            Right (FileStatus 11 True 20000)
        , expect $ DoesPathExist "/z/two" |-> Right False
        , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ DoesPathExist "/z/one" |-> Right False
        , expect $ MakeAbsolute "c/Haskell.mp4" |-> Right "/a/b/c/Haskell.mp4"
        , expect $ GetFileStatus "/a/b/c/Haskell.mp4" |->
            Right (FileStatus 11 False 40000)
        , expect $ DoesPathExist "/z/Haskell.mp4" |-> Right False
        , expect $ PutStrLn "cp /a/b/c/Haskell.mp4 /z/Haskell.mp4" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "mkdir /z/one" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ ListDirectory "/a/b/one" |->
            Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "cp /a/b/python.mp4 /z/python.mp4" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "mkdir /z/two" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ ListDirectory "/a/b/two" |-> Right []
        ]
      assertSuccess <=< Error.run $ run sourcesOptions
        { optCase   = CaseInsensitive
        , optScript = True
        }

------------------------------------------------------------------------------

testFirstNone :: TestTree
testFirstNone = testCase "FirstNone" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ CreateDirectory "/z/one" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "three", "Uno.mp3", "two", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/z/one/three" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/z/one/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []
      ]
    assertSuccess <=< Error.run $ run defaultOptions

------------------------------------------------------------------------------

testFirstNoneScript :: TestTree
testFirstNoneScript = testCase "FirstNoneScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ PutStrLn "mkdir /z/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "three", "Uno.mp3", "two", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /z/one/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /z/one/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optScript = True
      }

------------------------------------------------------------------------------

testFirstDirs :: TestTree
testFirstDirs = testCase "FirstDirs" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ CreateDirectory "/z/one" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "three", "Uno.mp3", "two", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ CreateDirectory "/z/one/three" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ CreateDirectory "/z/one/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []
      , expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst = FirstDirs
      }

------------------------------------------------------------------------------

testFirstDirsScript :: TestTree
testFirstDirsScript = testCase "FirstDirsScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ PutStrLn "mkdir /z/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "three", "Uno.mp3", "two", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "mkdir /z/one/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ PutStrLn "mkdir /z/one/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []
      , expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst  = FirstDirs
      , optScript = True
      }

------------------------------------------------------------------------------

testSourcesFirstDirs :: TestTree
testSourcesFirstDirs = testCase "SourcesFirstDirs" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "python.mp4" |-> Right "/a/b/python.mp4"
      , expect $ GetFileStatus "/a/b/python.mp4" |->
          Right (FileStatus 11 False 30000)
      , expect $ DoesPathExist "/z/python.mp4" |-> Right False
      , expect $ MakeAbsolute "/a/b/two" |-> Right "/a/b/two"
      , expect $ GetFileStatus "/a/b/two" |-> Right (FileStatus 11 True 20000)
      , expect $ DoesPathExist "/z/two" |-> Right False
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ MakeAbsolute "c/Haskell.mp4" |-> Right "/a/b/c/Haskell.mp4"
      , expect $ GetFileStatus "/a/b/c/Haskell.mp4" |->
          Right (FileStatus 11 False 40000)
      , expect $ DoesPathExist "/z/Haskell.mp4" |-> Right False
      , expect $ CreateDirectory "/z/one" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/z/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/two" |-> Right []
      , expect $ CopyFile "/a/b/c/Haskell.mp4" "/z/Haskell.mp4" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/python.mp4" "/z/python.mp4" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run sourcesOptions
      { optFirst = FirstDirs
      }

------------------------------------------------------------------------------

testSourcesFirstDirsScript :: TestTree
testSourcesFirstDirsScript = testCase "SourcesFirstDirsScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "python.mp4" |-> Right "/a/b/python.mp4"
      , expect $ GetFileStatus "/a/b/python.mp4" |->
          Right (FileStatus 11 False 30000)
      , expect $ DoesPathExist "/z/python.mp4" |-> Right False
      , expect $ MakeAbsolute "/a/b/two" |-> Right "/a/b/two"
      , expect $ GetFileStatus "/a/b/two" |->
          Right (FileStatus 11 True 20000)
      , expect $ DoesPathExist "/z/two" |-> Right False
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ MakeAbsolute "c/Haskell.mp4" |-> Right "/a/b/c/Haskell.mp4"
      , expect $ GetFileStatus "/a/b/c/Haskell.mp4" |->
          Right (FileStatus 11 False 40000)
      , expect $ DoesPathExist "/z/Haskell.mp4" |-> Right False
      , expect $ PutStrLn "mkdir /z/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /z/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/two" |-> Right []
      , expect $ PutStrLn "cp /a/b/c/Haskell.mp4 /z/Haskell.mp4" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/python.mp4 /z/python.mp4" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run sourcesOptions
      { optFirst  = FirstDirs
      , optScript = True
      }

------------------------------------------------------------------------------

testFirstFiles :: TestTree
testFirstFiles = testCase "FirstFiles" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ CreateDirectory "/z/one" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "three", "Uno.mp3", "two", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/z/one/three" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ CreateDirectory "/z/one/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst = FirstFiles
      }

------------------------------------------------------------------------------

testFirstFilesScript :: TestTree
testFirstFilesScript = testCase "FirstFilesScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ PutStrLn "mkdir /z/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "three", "Uno.mp3", "two", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /z/one/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ PutStrLn "mkdir /z/one/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []

      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst  = FirstFiles
      , optScript = True
      }

------------------------------------------------------------------------------

testSourcesFirstFiles :: TestTree
testSourcesFirstFiles = testCase "SourcesFirstFiles" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "python.mp4" |-> Right "/a/b/python.mp4"
      , expect $ GetFileStatus "/a/b/python.mp4" |->
          Right (FileStatus 11 False 30000)
      , expect $ DoesPathExist "/z/python.mp4" |-> Right False
      , expect $ MakeAbsolute "/a/b/two" |-> Right "/a/b/two"
      , expect $ GetFileStatus "/a/b/two" |-> Right (FileStatus 11 True 20000)
      , expect $ DoesPathExist "/z/two" |-> Right False
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ MakeAbsolute "c/Haskell.mp4" |-> Right "/a/b/c/Haskell.mp4"
      , expect $ GetFileStatus "/a/b/c/Haskell.mp4" |->
          Right (FileStatus 11 False 40000)
      , expect $ DoesPathExist "/z/Haskell.mp4" |-> Right False
      , expect $ CopyFile "/a/b/c/Haskell.mp4" "/z/Haskell.mp4" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/python.mp4" "/z/python.mp4" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/z/one" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/z/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/two" |-> Right []
      ]
    assertSuccess <=< Error.run $ run sourcesOptions
      { optFirst = FirstFiles
      }

------------------------------------------------------------------------------

testSourcesFirstFilesScript :: TestTree
testSourcesFirstFilesScript =
    testCase "SourcesFirstFilesScript" . runMockT $ do
      inSequence
        [ expect $ MakeAbsolute "/z" |-> Right "/z"
        , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
        , expect $ MakeAbsolute "python.mp4" |-> Right "/a/b/python.mp4"
        , expect $ GetFileStatus "/a/b/python.mp4" |->
            Right (FileStatus 11 False 30000)
        , expect $ DoesPathExist "/z/python.mp4" |-> Right False
        , expect $ MakeAbsolute "/a/b/two" |-> Right "/a/b/two"
        , expect $ GetFileStatus "/a/b/two" |->
            Right (FileStatus 11 True 20000)
        , expect $ DoesPathExist "/z/two" |-> Right False
        , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ DoesPathExist "/z/one" |-> Right False
        , expect $ MakeAbsolute "c/Haskell.mp4" |-> Right "/a/b/c/Haskell.mp4"
        , expect $ GetFileStatus "/a/b/c/Haskell.mp4" |->
            Right (FileStatus 11 False 40000)
        , expect $ DoesPathExist "/z/Haskell.mp4" |-> Right False
        , expect $ PutStrLn "cp /a/b/c/Haskell.mp4 /z/Haskell.mp4" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "cp /a/b/python.mp4 /z/python.mp4" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "mkdir /z/one" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ ListDirectory "/a/b/one" |->
            Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "mkdir /z/two" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ ListDirectory "/a/b/two" |-> Right []
        ]
      assertSuccess <=< Error.run $ run sourcesOptions
        { optFirst  = FirstFiles
        , optScript = True
        }

------------------------------------------------------------------------------

testNoSync :: TestTree
testNoSync = testCase "NoSync" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ CreateDirectory "/z/one" |-> Right ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |-> Right ()
      , expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |-> Right ()
      , expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |-> Right ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optSync = False
      }

------------------------------------------------------------------------------

testNoSyncScript :: TestTree
testNoSyncScript = testCase "NoSyncScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ PutStrLn "mkdir /z/one" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
      , expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optSync   = False
      , optScript = True
      }

------------------------------------------------------------------------------

testOrderNameReverse :: TestTree
testOrderNameReverse = testCase "OrderNameReverse" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ CreateDirectory "/z/one" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "three", "Uno.mp3", "two", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ CreateDirectory "/z/one/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []
      , expect $ CreateDirectory "/z/one/three" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst   = FirstDirs
      , optReverse = True
      }

------------------------------------------------------------------------------

testOrderNameReverseScript :: TestTree
testOrderNameReverseScript = testCase "OrderNameReverseScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ PutStrLn "mkdir /z/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "three", "Uno.mp3", "two", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "mkdir /z/one/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []
      , expect $ PutStrLn "mkdir /z/one/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst   = FirstDirs
      , optReverse = True
      , optScript  = True
      }

------------------------------------------------------------------------------

testSourcesOrderNameReverse :: TestTree
testSourcesOrderNameReverse =
    testCase "SourcesOrderNameReverse" . runMockT $ do
      inSequence
        [ expect $ MakeAbsolute "/z" |-> Right "/z"
        , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
        , expect $ MakeAbsolute "python.mp4" |-> Right "/a/b/python.mp4"
        , expect $ GetFileStatus "/a/b/python.mp4" |->
            Right (FileStatus 11 False 30000)
        , expect $ DoesPathExist "/z/python.mp4" |-> Right False
        , expect $ MakeAbsolute "/a/b/two" |-> Right "/a/b/two"
        , expect $ GetFileStatus "/a/b/two" |->
            Right (FileStatus 11 True 20000)
        , expect $ DoesPathExist "/z/two" |-> Right False
        , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ DoesPathExist "/z/one" |-> Right False
        , expect $ MakeAbsolute "c/Haskell.mp4" |-> Right "/a/b/c/Haskell.mp4"
        , expect $ GetFileStatus "/a/b/c/Haskell.mp4" |->
            Right (FileStatus 11 False 40000)
        , expect $ DoesPathExist "/z/Haskell.mp4" |-> Right False
        , expect $ CreateDirectory "/z/two" |-> Right ()
        , expect $ Sync |-> ()
        , expect $ ListDirectory "/a/b/two" |-> Right []
        , expect $ CreateDirectory "/z/one" |-> Right ()
        , expect $ Sync |-> ()
        , expect $ ListDirectory "/a/b/one" |->
            Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |-> Right ()
        , expect $ Sync |-> ()
        , expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |-> Right ()
        , expect $ Sync |-> ()
        , expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |-> Right ()
        , expect $ Sync |-> ()
        , expect $ CopyFile "/a/b/python.mp4" "/z/python.mp4" |-> Right ()
        , expect $ Sync |-> ()
        , expect $ CopyFile "/a/b/c/Haskell.mp4" "/z/Haskell.mp4" |-> Right ()
        , expect $ Sync |-> ()
        ]
      assertSuccess <=< Error.run $ run sourcesOptions
        { optFirst   = FirstDirs
        , optReverse = True
        }

------------------------------------------------------------------------------

testSourcesOrderNameReverseScript :: TestTree
testSourcesOrderNameReverseScript =
    testCase "SourcesOrderNameReverseScript" . runMockT $ do
      inSequence
        [ expect $ MakeAbsolute "/z" |-> Right "/z"
        , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
        , expect $ MakeAbsolute "python.mp4" |-> Right "/a/b/python.mp4"
        , expect $ GetFileStatus "/a/b/python.mp4" |->
            Right (FileStatus 11 False 30000)
        , expect $ DoesPathExist "/z/python.mp4" |-> Right False
        , expect $ MakeAbsolute "/a/b/two" |-> Right "/a/b/two"
        , expect $ GetFileStatus "/a/b/two" |->
            Right (FileStatus 11 True 20000)
        , expect $ DoesPathExist "/z/two" |-> Right False
        , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ DoesPathExist "/z/one" |-> Right False
        , expect $ MakeAbsolute "c/Haskell.mp4" |-> Right "/a/b/c/Haskell.mp4"
        , expect $ GetFileStatus "/a/b/c/Haskell.mp4" |->
            Right (FileStatus 11 False 40000)
        , expect $ DoesPathExist "/z/Haskell.mp4" |-> Right False
        , expect $ PutStrLn "mkdir /z/two" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ ListDirectory "/a/b/two" |-> Right []
        , expect $ PutStrLn "mkdir /z/one" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ ListDirectory "/a/b/one" |->
            Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 2000)
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 1000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "cp /a/b/python.mp4 /z/python.mp4" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "cp /a/b/c/Haskell.mp4 /z/Haskell.mp4" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        ]
      assertSuccess <=< Error.run $ run sourcesOptions
        { optFirst   = FirstDirs
        , optReverse = True
        , optScript  = True
        }

------------------------------------------------------------------------------

testOrderTime :: TestTree
testOrderTime = testCase "OrderTime" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ CreateDirectory "/z/one" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "three", "Uno.mp3", "two", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 20000)
      , expect $ GetFileStatus "/a/b/one/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 10000)
      , expect $ GetFileStatus "/a/b/one/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ CreateDirectory "/z/one/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []
      , expect $ CreateDirectory "/z/one/three" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst = FirstDirs
      , optOrder = OrderTime
      }

------------------------------------------------------------------------------

testOrderTimeScript :: TestTree
testOrderTimeScript = testCase "OrderTimeScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ PutStrLn "mkdir /z/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "three", "Uno.mp3", "two", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 20000)
      , expect $ GetFileStatus "/a/b/one/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 10000)
      , expect $ GetFileStatus "/a/b/one/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "mkdir /z/one/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []
      , expect $ PutStrLn "mkdir /z/one/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst  = FirstDirs
      , optOrder  = OrderTime
      , optScript = True
      }

------------------------------------------------------------------------------

testSourcesOrderTime :: TestTree
testSourcesOrderTime = testCase "SourcesOrderTime" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "python.mp4" |-> Right "/a/b/python.mp4"
      , expect $ GetFileStatus "/a/b/python.mp4" |->
          Right (FileStatus 11 False 30000)
      , expect $ DoesPathExist "/z/python.mp4" |-> Right False
      , expect $ MakeAbsolute "/a/b/two" |-> Right "/a/b/two"
      , expect $ GetFileStatus "/a/b/two" |-> Right (FileStatus 11 True 32000)
      , expect $ DoesPathExist "/z/two" |-> Right False
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 33000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ MakeAbsolute "c/Haskell.mp4" |-> Right "/a/b/c/Haskell.mp4"
      , expect $ GetFileStatus "/a/b/c/Haskell.mp4" |->
          Right (FileStatus 11 False 40000)
      , expect $ DoesPathExist "/z/Haskell.mp4" |-> Right False
      , expect $ CreateDirectory "/z/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/two" |-> Right []
      , expect $ CreateDirectory "/z/one" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 20000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 10000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/python.mp4" "/z/python.mp4" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/c/Haskell.mp4" "/z/Haskell.mp4" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run sourcesOptions
      { optFirst = FirstDirs
      , optOrder = OrderTime
      }

------------------------------------------------------------------------------

testSourcesOrderTimeScript :: TestTree
testSourcesOrderTimeScript = testCase "SourcesOrderTimeScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "python.mp4" |-> Right "/a/b/python.mp4"
      , expect $ GetFileStatus "/a/b/python.mp4" |->
          Right (FileStatus 11 False 30000)
      , expect $ DoesPathExist "/z/python.mp4" |-> Right False
      , expect $ MakeAbsolute "/a/b/two" |-> Right "/a/b/two"
      , expect $ GetFileStatus "/a/b/two" |-> Right (FileStatus 11 True 32000)
      , expect $ DoesPathExist "/z/two" |-> Right False
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 33000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ MakeAbsolute "c/Haskell.mp4" |-> Right "/a/b/c/Haskell.mp4"
      , expect $ GetFileStatus "/a/b/c/Haskell.mp4" |->
          Right (FileStatus 11 False 40000)
      , expect $ DoesPathExist "/z/Haskell.mp4" |-> Right False
      , expect $ PutStrLn "mkdir /z/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/two" |-> Right []
      , expect $ PutStrLn "mkdir /z/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 20000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 10000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/python.mp4 /z/python.mp4" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/c/Haskell.mp4 /z/Haskell.mp4" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run sourcesOptions
      { optFirst  = FirstDirs
      , optOrder  = OrderTime
      , optScript = True
      }

------------------------------------------------------------------------------

testOrderTimeReverse :: TestTree
testOrderTimeReverse = testCase "OrderTimeReverse" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ CreateDirectory "/z/one" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "three", "Uno.mp3", "two", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 20000)
      , expect $ GetFileStatus "/a/b/one/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 10000)
      , expect $ GetFileStatus "/a/b/one/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ CreateDirectory "/z/one/three" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ CreateDirectory "/z/one/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []
      , expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst   = FirstDirs
      , optOrder   = OrderTime
      , optReverse = True
      }

------------------------------------------------------------------------------

testOrderTimeReverseScript :: TestTree
testOrderTimeReverseScript = testCase "OrderTimeReverseScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ PutStrLn "mkdir /z/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "three", "Uno.mp3", "two", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 20000)
      , expect $ GetFileStatus "/a/b/one/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 10000)
      , expect $ GetFileStatus "/a/b/one/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "mkdir /z/one/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ PutStrLn "mkdir /z/one/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []
      , expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst   = FirstDirs
      , optOrder   = OrderTime
      , optReverse = True
      , optScript  = True
      }

------------------------------------------------------------------------------

testSourcesOrderTimeReverse :: TestTree
testSourcesOrderTimeReverse =
    testCase "SourcesOrderTimeReverse" . runMockT $ do
      inSequence
        [ expect $ MakeAbsolute "/z" |-> Right "/z"
        , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
        , expect $ MakeAbsolute "python.mp4" |-> Right "/a/b/python.mp4"
        , expect $ GetFileStatus "/a/b/python.mp4" |->
            Right (FileStatus 11 False 30000)
        , expect $ DoesPathExist "/z/python.mp4" |-> Right False
        , expect $ MakeAbsolute "/a/b/two" |-> Right "/a/b/two"
        , expect $ GetFileStatus "/a/b/two" |->
            Right (FileStatus 11 True 32000)
        , expect $ DoesPathExist "/z/two" |-> Right False
        , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 33000)
        , expect $ DoesPathExist "/z/one" |-> Right False
        , expect $ MakeAbsolute "c/Haskell.mp4" |-> Right "/a/b/c/Haskell.mp4"
        , expect $ GetFileStatus "/a/b/c/Haskell.mp4" |->
            Right (FileStatus 11 False 40000)
        , expect $ DoesPathExist "/z/Haskell.mp4" |-> Right False
        , expect $ CreateDirectory "/z/one" |-> Right ()
        , expect $ Sync |-> ()
        , expect $ ListDirectory "/a/b/one" |->
            Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 20000)
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 10000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |-> Right ()
        , expect $ Sync |-> ()
        , expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |-> Right ()
        , expect $ Sync |-> ()
        , expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |-> Right ()
        , expect $ Sync |-> ()
        , expect $ CreateDirectory "/z/two" |-> Right ()
        , expect $ Sync |-> ()
        , expect $ ListDirectory "/a/b/two" |-> Right []
        , expect $ CopyFile "/a/b/c/Haskell.mp4" "/z/Haskell.mp4" |-> Right ()
        , expect $ Sync |-> ()
        , expect $ CopyFile "/a/b/python.mp4" "/z/python.mp4" |-> Right ()
        , expect $ Sync |-> ()
        ]
      assertSuccess <=< Error.run $ run sourcesOptions
        { optFirst   = FirstDirs
        , optOrder   = OrderTime
        , optReverse = True
        }

------------------------------------------------------------------------------

testSourcesOrderTimeReverseScript :: TestTree
testSourcesOrderTimeReverseScript =
    testCase "SourcesOrderTimeReverseScript" . runMockT $ do
      inSequence
        [ expect $ MakeAbsolute "/z" |-> Right "/z"
        , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
        , expect $ MakeAbsolute "python.mp4" |-> Right "/a/b/python.mp4"
        , expect $ GetFileStatus "/a/b/python.mp4" |->
            Right (FileStatus 11 False 30000)
        , expect $ DoesPathExist "/z/python.mp4" |-> Right False
        , expect $ MakeAbsolute "/a/b/two" |-> Right "/a/b/two"
        , expect $ GetFileStatus "/a/b/two" |->
            Right (FileStatus 11 True 32000)
        , expect $ DoesPathExist "/z/two" |-> Right False
        , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 33000)
        , expect $ DoesPathExist "/z/one" |-> Right False
        , expect $ MakeAbsolute "c/Haskell.mp4" |-> Right "/a/b/c/Haskell.mp4"
        , expect $ GetFileStatus "/a/b/c/Haskell.mp4" |->
            Right (FileStatus 11 False 40000)
        , expect $ DoesPathExist "/z/Haskell.mp4" |-> Right False
        , expect $ PutStrLn "mkdir /z/one" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ ListDirectory "/a/b/one" |->
            Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
        , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
            Right (FileStatus 11 False 20000)
        , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
            Right (FileStatus 11 False 10000)
        , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
            Right (FileStatus 11 False 3000)
        , expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "mkdir /z/two" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ ListDirectory "/a/b/two" |-> Right []
        , expect $ PutStrLn "cp /a/b/c/Haskell.mp4 /z/Haskell.mp4" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        , expect $ PutStrLn "cp /a/b/python.mp4 /z/python.mp4" |-> ()
        , expect $ PutStrLn "sync" |-> ()
        ]
      assertSuccess <=< Error.run $ run sourcesOptions
        { optFirst   = FirstDirs
        , optOrder   = OrderTime
        , optReverse = True
        , optScript  = True
        }

------------------------------------------------------------------------------

testOrderRandom :: TestTree
testOrderRandom = testCase "OrderRandom" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ CreateDirectory "/z/one" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "three", "Uno.mp3", "two", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , inAnyOrder
          [ inSequence
              [ expect $ CreateDirectory "/z/one/three" |-> Right ()
              , expect $ Sync |-> ()
              , expect $ ListDirectory "/a/b/one/three" |-> Right []
              ]
          , inSequence
              [ expect $ CreateDirectory "/z/one/two" |-> Right ()
              , expect $ Sync |-> ()
              , expect $ ListDirectory "/a/b/one/two" |-> Right []
              ]
          ]
      , inAnyOrder
          [ inSequence
              [ expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |->
                  Right ()
              , expect $ Sync |-> ()
              ]
          , inSequence
              [ expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |->
                  Right ()
              , expect $ Sync |-> ()
              ]
          , inSequence
              [ expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |->
                  Right ()
              , expect $ Sync |-> ()
              ]
          ]
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst = FirstDirs
      , optOrder = OrderRandom
      }

------------------------------------------------------------------------------

testOrderRandomScript :: TestTree
testOrderRandomScript = testCase "OrderRandomScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ PutStrLn "mkdir /z/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "three", "Uno.mp3", "two", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , inAnyOrder
          [ inSequence
              [ expect $ PutStrLn "mkdir /z/one/three" |-> ()
              , expect $ PutStrLn "sync" |-> ()
              , expect $ ListDirectory "/a/b/one/three" |-> Right []
              ]
          , inSequence
              [ expect $ PutStrLn "mkdir /z/one/two" |-> ()
              , expect $ PutStrLn "sync" |-> ()
              , expect $ ListDirectory "/a/b/one/two" |-> Right []
              ]
          ]
      , inAnyOrder
          [ inSequence
              [ expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
              , expect $ PutStrLn "sync" |-> ()
              ]
          , inSequence
              [ expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
              , expect $ PutStrLn "sync" |-> ()
              ]
          , inSequence
              [ expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |->
                  ()
              , expect $ PutStrLn "sync" |-> ()
              ]
          ]
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst  = FirstDirs
      , optOrder  = OrderRandom
      , optScript = True
      }

------------------------------------------------------------------------------

testSourcesOrderRandom :: TestTree
testSourcesOrderRandom = testCase "SourcesOrderRandom" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "python.mp4" |-> Right "/a/b/python.mp4"
      , expect $ GetFileStatus "/a/b/python.mp4" |->
          Right (FileStatus 11 False 30000)
      , expect $ DoesPathExist "/z/python.mp4" |-> Right False
      , expect $ MakeAbsolute "/a/b/two" |-> Right "/a/b/two"
      , expect $ GetFileStatus "/a/b/two" |-> Right (FileStatus 11 True 20000)
      , expect $ DoesPathExist "/z/two" |-> Right False
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ MakeAbsolute "c/Haskell.mp4" |-> Right "/a/b/c/Haskell.mp4"
      , expect $ GetFileStatus "/a/b/c/Haskell.mp4" |->
          Right (FileStatus 11 False 40000)
      , expect $ DoesPathExist "/z/Haskell.mp4" |-> Right False
      , inAnyOrder
          [ inSequence
              [ expect $ CreateDirectory "/z/one" |-> Right ()
              , expect $ Sync |-> ()
              , expect $ ListDirectory "/a/b/one" |->
                  Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
              , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
                  Right (FileStatus 11 False 2000)
              , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
                  Right (FileStatus 11 False 1000)
              , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
                  Right (FileStatus 11 False 3000)
              , inAnyOrder
                  [ inSequence
                      [ expect $
                          CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |->
                            Right ()
                      , expect $ Sync |-> ()
                      ]
                  , inSequence
                      [ expect $
                          CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |->
                            Right ()
                      , expect $ Sync |-> ()
                      ]
                  , inSequence
                      [ expect $
                          CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |->
                            Right ()
                      , expect $ Sync |-> ()
                      ]
                  ]
              ]
          , inSequence
              [ expect $ CreateDirectory "/z/two" |-> Right ()
              , expect $ Sync |-> ()
              , expect $ ListDirectory "/a/b/two" |-> Right []
              ]
          ]
      , inAnyOrder
          [ inSequence
              [ expect $ CopyFile "/a/b/c/Haskell.mp4" "/z/Haskell.mp4" |->
                  Right ()
              , expect $ Sync |-> ()
              ]
          , inSequence
              [ expect $ CopyFile "/a/b/python.mp4" "/z/python.mp4" |->
                  Right ()
              , expect $ Sync |-> ()
              ]
          ]
      ]
    assertSuccess <=< Error.run $ run sourcesOptions
      { optFirst = FirstDirs
      , optOrder = OrderRandom
      }

------------------------------------------------------------------------------

testSourcesOrderRandomScript :: TestTree
testSourcesOrderRandomScript =
    testCase "SourcesOrderRandomScript" . runMockT $ do
      inSequence
        [ expect $ MakeAbsolute "/z" |-> Right "/z"
        , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
        , expect $ MakeAbsolute "python.mp4" |-> Right "/a/b/python.mp4"
        , expect $ GetFileStatus "/a/b/python.mp4" |->
            Right (FileStatus 11 False 30000)
        , expect $ DoesPathExist "/z/python.mp4" |-> Right False
        , expect $ MakeAbsolute "/a/b/two" |-> Right "/a/b/two"
        , expect $ GetFileStatus "/a/b/two" |->
            Right (FileStatus 11 True 20000)
        , expect $ DoesPathExist "/z/two" |-> Right False
        , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
        , expect $ GetFileStatus "/a/b/one" |->
            Right (FileStatus 11 True 10000)
        , expect $ DoesPathExist "/z/one" |-> Right False
        , expect $ MakeAbsolute "c/Haskell.mp4" |-> Right "/a/b/c/Haskell.mp4"
        , expect $ GetFileStatus "/a/b/c/Haskell.mp4" |->
            Right (FileStatus 11 False 40000)
        , expect $ DoesPathExist "/z/Haskell.mp4" |-> Right False
        , inAnyOrder
            [ inSequence
                [ expect $ PutStrLn "mkdir /z/one" |-> ()
                , expect $ PutStrLn "sync" |-> ()
                , expect $ ListDirectory "/a/b/one" |->
                    Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
                , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
                    Right (FileStatus 11 False 2000)
                , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
                    Right (FileStatus 11 False 1000)
                , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
                    Right (FileStatus 11 False 3000)
                , inAnyOrder
                    [ inSequence
                        [ expect $
                            PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |->
                              ()
                        , expect $ PutStrLn "sync" |-> ()
                        ]
                    , inSequence
                        [ expect $
                            PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |->
                              ()
                        , expect $ PutStrLn "sync" |-> ()
                        ]
                    , inSequence
                        [ expect $
                            PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3"
                            |-> ()
                        , expect $ PutStrLn "sync" |-> ()
                        ]
                    ]
                ]
            , inSequence
                [ expect $ PutStrLn "mkdir /z/two" |-> ()
                , expect $ PutStrLn "sync" |-> ()
                , expect $ ListDirectory "/a/b/two" |-> Right []
                ]
            ]
        , inAnyOrder
            [ inSequence
                [ expect $ PutStrLn "cp /a/b/c/Haskell.mp4 /z/Haskell.mp4" |->
                    ()
                , expect $ PutStrLn "sync" |-> ()
                ]
            , inSequence
                [ expect $ PutStrLn "cp /a/b/python.mp4 /z/python.mp4" |-> ()
                , expect $ PutStrLn "sync" |-> ()
                ]
            ]
        ]
      assertSuccess <=< Error.run $ run sourcesOptions
        { optFirst  = FirstDirs
        , optOrder  = OrderRandom
        , optScript = True
        }

------------------------------------------------------------------------------

testVerbose :: TestTree
testVerbose = testCase "Verbose" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ PutStrLn "one" |-> ()
      , expect $ CreateDirectory "/z/one" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "one/Uno.mp3" |-> ()
      , expect $ CopyFile "/a/b/one/Uno.mp3" "/z/one/Uno.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ PutStrLn "one/dos.mp3" |-> ()
      , expect $ CopyFile "/a/b/one/dos.mp3" "/z/one/dos.mp3" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ PutStrLn "one/tres.mp3" |-> ()
      , expect $ CopyFile "/a/b/one/tres.mp3" "/z/one/tres.mp3" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optVerbose = True
      }

------------------------------------------------------------------------------

testVerboseScript :: TestTree
testVerboseScript = testCase "VerboseScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right False
      , expect $ PutStrLn "echo one" |-> ()
      , expect $ PutStrLn "mkdir /z/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["dos.mp3", "Uno.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "echo one/Uno.mp3" |-> ()
      , expect $ PutStrLn "cp /a/b/one/Uno.mp3 /z/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "echo one/dos.mp3" |-> ()
      , expect $ PutStrLn "cp /a/b/one/dos.mp3 /z/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "echo one/tres.mp3" |-> ()
      , expect $ PutStrLn "cp /a/b/one/tres.mp3 /z/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optScript  = True
      , optVerbose = True
      }

------------------------------------------------------------------------------

testDestinationNotFound :: TestTree
testDestinationNotFound = testCase "DestinationNotFound" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Left (userError "file not found: /z")
      ]
    assertError "user error (file not found: /z)" <=< Error.run $
      run defaultOptions

------------------------------------------------------------------------------

testDestinationNotDirectory :: TestTree
testDestinationNotDirectory =
    testCase "DestinationNotDirectory" . runMockT $ do
      inSequence
        [ expect $ MakeAbsolute "/z" |-> Right "/z"
        , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 False 90000)
        ]
      assertError "not a directory: /z" <=< Error.run $ run defaultOptions

------------------------------------------------------------------------------

testSourceNotFound :: TestTree
testSourceNotFound = testCase "SourceNotFound" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Left (userError "file not found: /a/b/one")
      ]
    assertError "user error (file not found: /a/b/one)" <=< Error.run $
      run defaultOptions

------------------------------------------------------------------------------

testSourceExists :: TestTree
testSourceExists = testCase "SourceExists" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |-> Right (FileStatus 11 True 10000)
      , expect $ DoesPathExist "/z/one" |-> Right True
      ]
    assertError "already exists: /z/one" <=< Error.run $ run defaultOptions

------------------------------------------------------------------------------

testLarge :: TestTree
testLarge = testCase "Large" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "d" |-> Right "/a/b/d"
      , expect $ GetFileStatus "/a/b/d" |-> Right (FileStatus 11 True 2000)
      , expect $ DoesPathExist "/z/d" |-> Right False
      , expect $ MakeAbsolute "c" |-> Right "/a/b/c"
      , expect $ GetFileStatus "/a/b/c" |-> Right (FileStatus 11 True 1000)
      , expect $ DoesPathExist "/z/c" |-> Right False
      , expect $ CreateDirectory "/z/c" |-> Right ()
      , expect $ ListDirectory "/a/b/c" |-> Right ["two", "one"]
      , expect $ GetFileStatus "/a/b/c/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/c/one" |->
          Right (FileStatus 11 True 1000)
      , expect $ CreateDirectory "/z/c/one" |-> Right ()
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
      , expect $ CopyFile "/a/b/c/one/11.mp3" "/z/c/one/11.mp3" |-> Right ()
      , expect $ CopyFile "/a/b/c/one/12.mp3" "/z/c/one/12.mp3" |-> Right ()
      , expect $ CopyFile "/a/b/c/one/13.mp3" "/z/c/one/13.mp3" |-> Right ()
      , expect $ CopyFile "/a/b/c/one/14.mp3" "/z/c/one/14.mp3" |-> Right ()
      , expect $ CreateDirectory "/z/c/one/eight" |-> Right ()
      , expect $ ListDirectory "/a/b/c/one/eight" |->
          Right ["83.mp3", "81.mp3", "82.mp3"]
      , expect $ GetFileStatus "/a/b/c/one/eight/83.mp3" |->
          Right (FileStatus 11 False 8300)
      , expect $ GetFileStatus "/a/b/c/one/eight/81.mp3" |->
          Right (FileStatus 11 False 8100)
      , expect $ GetFileStatus "/a/b/c/one/eight/82.mp3" |->
          Right (FileStatus 11 False 8200)
      , expect $
          CopyFile "/a/b/c/one/eight/81.mp3" "/z/c/one/eight/81.mp3" |->
            Right ()
      , expect $
          CopyFile "/a/b/c/one/eight/82.mp3" "/z/c/one/eight/82.mp3" |->
            Right ()
      , expect $
          CopyFile "/a/b/c/one/eight/83.mp3" "/z/c/one/eight/83.mp3" |->
            Right ()
      , expect $ CreateDirectory "/z/c/one/six" |-> Right ()
      , expect $ ListDirectory "/a/b/c/one/six" |-> Right []
      , expect $ CreateDirectory "/z/c/two" |-> Right ()
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
      , expect $ CopyFile "/a/b/c/two/21.mp3" "/z/c/two/21.mp3" |-> Right ()
      , expect $ CopyFile "/a/b/c/two/22.mp3" "/z/c/two/22.mp3" |-> Right ()
      , expect $ CopyFile "/a/b/c/two/23.mp3" "/z/c/two/23.mp3" |-> Right ()
      , expect $ CreateDirectory "/z/c/two/five" |-> Right ()
      , expect $ ListDirectory "/a/b/c/two/five" |-> Right ["51.mp3"]
      , expect $ GetFileStatus "/a/b/c/two/five/51.mp3" |->
          Right (FileStatus 11 False 5100)
      , expect $ CopyFile "/a/b/c/two/five/51.mp3" "/z/c/two/five/51.mp3" |->
          Right ()
      , expect $ CreateDirectory "/z/c/two/seven" |-> Right ()
      , expect $ ListDirectory "/a/b/c/two/seven" |->
          Right ["72.mp3", "71.mp3"]
      , expect $ GetFileStatus "/a/b/c/two/seven/72.mp3" |->
          Right (FileStatus 11 False 7200)
      , expect $ GetFileStatus "/a/b/c/two/seven/71.mp3" |->
          Right (FileStatus 11 False 7100)
      , expect $
          CopyFile "/a/b/c/two/seven/71.mp3" "/z/c/two/seven/71.mp3" |->
            Right ()
      , expect $
          CopyFile "/a/b/c/two/seven/72.mp3" "/z/c/two/seven/72.mp3" |->
            Right ()
      , expect $ CreateDirectory "/z/d" |-> Right ()
      , expect $ ListDirectory "/a/b/d" |-> Right ["three"]
      , expect $ GetFileStatus "/a/b/d/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ CreateDirectory "/z/d/three" |-> Right ()
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
      , expect $ CopyFile "/a/b/d/three/31.mp3" "/z/d/three/31.mp3" |->
          Right ()
      , expect $ CopyFile "/a/b/d/three/32.mp3" "/z/d/three/32.mp3" |->
          Right ()
      , expect $ CopyFile "/a/b/d/three/33.mp3" "/z/d/three/33.mp3" |->
          Right ()
      , expect $ CopyFile "/a/b/d/three/34.mp3" "/z/d/three/34.mp3" |->
          Right ()
      , expect $ CreateDirectory "/z/d/three/four" |-> Right ()
      , expect $ ListDirectory "/a/b/d/three/four" |->
          Right ["41.mp3", "42.mp3"]
      , expect $ GetFileStatus "/a/b/d/three/four/41.mp3" |->
          Right (FileStatus 11 False 4100)
      , expect $ GetFileStatus "/a/b/d/three/four/42.mp3" |->
          Right (FileStatus 11 False 4200)
      , expect $
          CopyFile "/a/b/d/three/four/41.mp3" "/z/d/three/four/41.mp3" |->
            Right ()
      , expect $
          CopyFile "/a/b/d/three/four/42.mp3" "/z/d/three/four/42.mp3" |->
            Right ()
      , expect $ CreateDirectory "/z/d/three/nine" |-> Right ()
      , expect $ ListDirectory "/a/b/d/three/nine" |-> Right ["ten"]
      , expect $ GetFileStatus "/a/b/d/three/nine/ten" |->
          Right (FileStatus 11 True 10000)
      , expect $ CreateDirectory "/z/d/three/nine/ten" |-> Right ()
      , expect $ ListDirectory "/a/b/d/three/nine/ten" |->
          Right ["102.mp3", "101.mp3"]
      , expect $ GetFileStatus "/a/b/d/three/nine/ten/102.mp3" |->
          Right (FileStatus 11 False 10200)
      , expect $ GetFileStatus "/a/b/d/three/nine/ten/101.mp3" |->
          Right (FileStatus 11 False 10100)
      , expect $
          CopyFile
            "/a/b/d/three/nine/ten/101.mp3"
            "/z/d/three/nine/ten/101.mp3"
            |-> Right ()
      , expect $
          CopyFile
            "/a/b/d/three/nine/ten/102.mp3"
            "/z/d/three/nine/ten/102.mp3"
            |-> Right ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optSync    = False
      , optSources = NonEmpty.fromList ["d", "c"]
      }

------------------------------------------------------------------------------

testLargeScript :: TestTree
testLargeScript = testCase "LargeScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "/z" |-> Right "/z"
      , expect $ GetFileStatus "/z" |-> Right (FileStatus 10 True 90000)
      , expect $ MakeAbsolute "d" |-> Right "/a/b/d"
      , expect $ GetFileStatus "/a/b/d" |-> Right (FileStatus 11 True 2000)
      , expect $ DoesPathExist "/z/d" |-> Right False
      , expect $ MakeAbsolute "c" |-> Right "/a/b/c"
      , expect $ GetFileStatus "/a/b/c" |-> Right (FileStatus 11 True 1000)
      , expect $ DoesPathExist "/z/c" |-> Right False
      , expect $ PutStrLn "mkdir /z/c" |-> ()
      , expect $ ListDirectory "/a/b/c" |-> Right ["two", "one"]
      , expect $ GetFileStatus "/a/b/c/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/c/one" |->
          Right (FileStatus 11 True 1000)
      , expect $ PutStrLn "mkdir /z/c/one" |-> ()
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
      , expect $ PutStrLn "cp /a/b/c/one/11.mp3 /z/c/one/11.mp3" |-> ()
      , expect $ PutStrLn "cp /a/b/c/one/12.mp3 /z/c/one/12.mp3" |-> ()
      , expect $ PutStrLn "cp /a/b/c/one/13.mp3 /z/c/one/13.mp3" |-> ()
      , expect $ PutStrLn "cp /a/b/c/one/14.mp3 /z/c/one/14.mp3" |-> ()
      , expect $ PutStrLn "mkdir /z/c/one/eight" |-> ()
      , expect $ ListDirectory "/a/b/c/one/eight" |->
          Right ["83.mp3", "81.mp3", "82.mp3"]
      , expect $ GetFileStatus "/a/b/c/one/eight/83.mp3" |->
          Right (FileStatus 11 False 8300)
      , expect $ GetFileStatus "/a/b/c/one/eight/81.mp3" |->
          Right (FileStatus 11 False 8100)
      , expect $ GetFileStatus "/a/b/c/one/eight/82.mp3" |->
          Right (FileStatus 11 False 8200)
      , expect $
          PutStrLn "cp /a/b/c/one/eight/81.mp3 /z/c/one/eight/81.mp3" |-> ()
      , expect $
          PutStrLn "cp /a/b/c/one/eight/82.mp3 /z/c/one/eight/82.mp3" |-> ()
      , expect $
          PutStrLn "cp /a/b/c/one/eight/83.mp3 /z/c/one/eight/83.mp3" |-> ()
      , expect $ PutStrLn "mkdir /z/c/one/six" |-> ()
      , expect $ ListDirectory "/a/b/c/one/six" |-> Right []
      , expect $ PutStrLn "mkdir /z/c/two" |-> ()
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
      , expect $ PutStrLn "cp /a/b/c/two/21.mp3 /z/c/two/21.mp3" |-> ()
      , expect $ PutStrLn "cp /a/b/c/two/22.mp3 /z/c/two/22.mp3" |-> ()
      , expect $ PutStrLn "cp /a/b/c/two/23.mp3 /z/c/two/23.mp3" |-> ()
      , expect $ PutStrLn "mkdir /z/c/two/five" |-> ()
      , expect $ ListDirectory "/a/b/c/two/five" |-> Right ["51.mp3"]
      , expect $ GetFileStatus "/a/b/c/two/five/51.mp3" |->
          Right (FileStatus 11 False 5100)
      , expect $ PutStrLn "cp /a/b/c/two/five/51.mp3 /z/c/two/five/51.mp3" |->
          ()
      , expect $ PutStrLn "mkdir /z/c/two/seven" |-> ()
      , expect $ ListDirectory "/a/b/c/two/seven" |->
          Right ["72.mp3", "71.mp3"]
      , expect $ GetFileStatus "/a/b/c/two/seven/72.mp3" |->
          Right (FileStatus 11 False 7200)
      , expect $ GetFileStatus "/a/b/c/two/seven/71.mp3" |->
          Right (FileStatus 11 False 7100)
      , expect $
          PutStrLn "cp /a/b/c/two/seven/71.mp3 /z/c/two/seven/71.mp3" |-> ()
      , expect $
          PutStrLn "cp /a/b/c/two/seven/72.mp3 /z/c/two/seven/72.mp3" |-> ()
      , expect $ PutStrLn "mkdir /z/d" |-> ()
      , expect $ ListDirectory "/a/b/d" |-> Right ["three"]
      , expect $ GetFileStatus "/a/b/d/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ PutStrLn "mkdir /z/d/three" |-> ()
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
      , expect $ PutStrLn "cp /a/b/d/three/31.mp3 /z/d/three/31.mp3" |-> ()
      , expect $ PutStrLn "cp /a/b/d/three/32.mp3 /z/d/three/32.mp3" |-> ()
      , expect $ PutStrLn "cp /a/b/d/three/33.mp3 /z/d/three/33.mp3" |-> ()
      , expect $ PutStrLn "cp /a/b/d/three/34.mp3 /z/d/three/34.mp3" |-> ()
      , expect $ PutStrLn "mkdir /z/d/three/four" |-> ()
      , expect $ ListDirectory "/a/b/d/three/four" |->
          Right ["41.mp3", "42.mp3"]
      , expect $ GetFileStatus "/a/b/d/three/four/41.mp3" |->
          Right (FileStatus 11 False 4100)
      , expect $ GetFileStatus "/a/b/d/three/four/42.mp3" |->
          Right (FileStatus 11 False 4200)
      , expect $
          PutStrLn "cp /a/b/d/three/four/41.mp3 /z/d/three/four/41.mp3" |-> ()
      , expect $
          PutStrLn "cp /a/b/d/three/four/42.mp3 /z/d/three/four/42.mp3" |-> ()
      , expect $ PutStrLn "mkdir /z/d/three/nine" |-> ()
      , expect $ ListDirectory "/a/b/d/three/nine" |-> Right ["ten"]
      , expect $ GetFileStatus "/a/b/d/three/nine/ten" |->
          Right (FileStatus 11 True 10000)
      , expect $ PutStrLn "mkdir /z/d/three/nine/ten" |-> ()
      , expect $ ListDirectory "/a/b/d/three/nine/ten" |->
          Right ["102.mp3", "101.mp3"]
      , expect $ GetFileStatus "/a/b/d/three/nine/ten/102.mp3" |->
          Right (FileStatus 11 False 10200)
      , expect $ GetFileStatus "/a/b/d/three/nine/ten/101.mp3" |->
          Right (FileStatus 11 False 10100)
      , expect $
          PutStrLn
            "cp /a/b/d/three/nine/ten/101.mp3 /z/d/three/nine/ten/101.mp3"
            |-> ()
      , expect $
          PutStrLn
            "cp /a/b/d/three/nine/ten/102.mp3 /z/d/three/nine/ten/102.mp3"
            |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optSync    = False
      , optScript  = True
      , optSources = NonEmpty.fromList ["d", "c"]
      }

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "seqcp:HMock"
    [ testCaseSensitive
    , testCaseSensitiveScript
    , testSourcesCaseSensitive
    , testSourcesCaseSensitiveScript
    , testCaseInsensitive
    , testCaseInsensitiveScript
    , testSourcesCaseInsensitive
    , testSourcesCaseInsensitiveScript
    , testFirstNone
    , testFirstNoneScript
    , testFirstDirs
    , testFirstDirsScript
    , testSourcesFirstDirs
    , testSourcesFirstDirsScript
    , testFirstFiles
    , testFirstFilesScript
    , testSourcesFirstFiles
    , testSourcesFirstFilesScript
    , testNoSync
    , testNoSyncScript
    , testOrderNameReverse
    , testOrderNameReverseScript
    , testSourcesOrderNameReverse
    , testSourcesOrderNameReverseScript
    , testOrderTime
    , testOrderTimeScript
    , testSourcesOrderTime
    , testSourcesOrderTimeScript
    , testOrderTimeReverse
    , testOrderTimeReverseScript
    , testSourcesOrderTimeReverse
    , testSourcesOrderTimeReverseScript
    , testOrderRandom
    , testOrderRandomScript
    , testSourcesOrderRandom
    , testSourcesOrderRandomScript
    , testVerbose
    , testVerboseScript
    , testDestinationNotFound
    , testDestinationNotDirectory
    , testSourceNotFound
    , testSourceExists
    , testLarge
    , testLargeScript
    ]

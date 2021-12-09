module PhatSort.Cmd.PhatSort.HMock (tests) where

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
import PhatSort.Cmd.PhatSort
  ( Options
      ( Options, optCase, optFirst, optOrder, optReverse, optScript, optSync
      , optTargets, optVerbose
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
    { optCase    = CaseSensitive
    , optFirst   = FirstNone
    , optSync    = True
    , optOrder   = OrderName
    , optReverse = False
    , optScript  = False
    , optVerbose = False
    , optTargets = NonEmpty.fromList ["one"]
    }

------------------------------------------------------------------------------

testCaseSensitive :: TestTree
testCaseSensitive = testCase "CaseSensitive" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one" |-> Right ()
      , expect $ Sync |-> ()
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
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions

testCaseSensitiveScript :: TestTree
testCaseSensitiveScript = testCase "CaseSensitiveScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ PutStrLn "mv /a/b/one /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["Uno.mp3", "dos.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "rmdir /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optScript = True
      }

------------------------------------------------------------------------------

testCaseInsensitive :: TestTree
testCaseInsensitive = testCase "CaseInsensitive" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one" |-> Right ()
      , expect $ Sync |-> ()
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
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/Uno.mp3" "/a/b/one/Uno.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optCase = CaseInsensitive
      }

testCaseInsensitiveScript :: TestTree
testCaseInsensitiveScript = testCase "CaseInsensitiveScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ PutStrLn "mv /a/b/one /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["Uno.mp3", "dos.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "rmdir /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optCase   = CaseInsensitive
      , optScript = True
      }

------------------------------------------------------------------------------

testFirstNone :: TestTree
testFirstNone = testCase "FirstNone" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one" |-> Right ()
      , expect $ Sync |-> ()
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
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one/three" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one-phat/three" |-> Right []
      , expect $ RemoveDirectory "/a/b/one-phat/three" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one-phat/two" |-> Right []
      , expect $ RemoveDirectory "/a/b/one-phat/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions

testFirstNoneScript :: TestTree
testFirstNoneScript = testCase "FirstNoneScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ PutStrLn "mv /a/b/one /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
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
      , expect $ PutStrLn "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ PutStrLn "rmdir /a/b/one-phat/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []
      , expect $ PutStrLn "rmdir /a/b/one-phat/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "rmdir /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optScript = True
      }

------------------------------------------------------------------------------

testFirstDirs :: TestTree
testFirstDirs = testCase "FirstDirs" . runMockT$ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one" |-> Right ()
      , expect $ Sync |-> ()
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
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one-phat/three" |-> Right []
      , expect $ RemoveDirectory "/a/b/one-phat/three" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one-phat/two" |-> Right []
      , expect $ RemoveDirectory "/a/b/one-phat/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/Uno.mp3" "/a/b/one/Uno.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst = FirstDirs
      }

testFirstDirsScript :: TestTree
testFirstDirsScript = testCase "FirstDirsScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ PutStrLn "mv /a/b/one /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
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
      , expect $ PutStrLn "mkdir /a/b/one/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ PutStrLn "rmdir /a/b/one-phat/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []
      , expect $ PutStrLn "rmdir /a/b/one-phat/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "rmdir /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst  = FirstDirs
      , optScript = True
      }

------------------------------------------------------------------------------

testFirstFiles :: TestTree
testFirstFiles = testCase "FirstFiles" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one" |-> Right ()
      , expect $ Sync |-> ()
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
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one/three" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one-phat/three" |-> Right []
      , expect $ RemoveDirectory "/a/b/one-phat/three" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one-phat/two" |-> Right []
      , expect $ RemoveDirectory "/a/b/one-phat/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst = FirstFiles
      }

testFirstFilesScript :: TestTree
testFirstFilesScript = testCase "FirstFilesScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ PutStrLn "mv /a/b/one /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
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
      , expect $ PutStrLn "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ PutStrLn "rmdir /a/b/one-phat/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []
      , expect $ PutStrLn "rmdir /a/b/one-phat/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "rmdir /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst  = FirstFiles
      , optScript = True
      }

------------------------------------------------------------------------------

testNoSync :: TestTree
testNoSync = testCase "NoSync" . runMockT $ do
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
    assertSuccess <=< Error.run $ run defaultOptions
      { optSync = False
      }

testNoSyncScript :: TestTree
testNoSyncScript = testCase "NoSyncScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ PutStrLn "mv /a/b/one /a/b/one-phat" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["Uno.mp3", "dos.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3" |-> ()
      , expect $ PutStrLn "rmdir /a/b/one-phat" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optSync   = False
      , optScript = True
      }

------------------------------------------------------------------------------

testOrderNameReverse :: TestTree
testOrderNameReverse = testCase "OrderNameReverse" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one" |-> Right ()
      , expect $ Sync |-> ()
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
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one-phat/two" |-> Right []
      , expect $ RemoveDirectory "/a/b/one-phat/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one/three" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one-phat/three" |-> Right []
      , expect $ RemoveDirectory "/a/b/one-phat/three" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/Uno.mp3" "/a/b/one/Uno.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst   = FirstDirs
      , optReverse = True
      }

testOrderNameReverseScript :: TestTree
testOrderNameReverseScript = testCase "OrderNameReverseScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ PutStrLn "mv /a/b/one /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
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
      , expect $ PutStrLn "mkdir /a/b/one/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []
      , expect $ PutStrLn "rmdir /a/b/one-phat/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ PutStrLn "rmdir /a/b/one-phat/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "rmdir /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst   = FirstDirs
      , optReverse = True
      , optScript  = True
      }

------------------------------------------------------------------------------

testOrderTime :: TestTree
testOrderTime = testCase "OrderTime" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one" |-> Right ()
      , expect $ Sync |-> ()
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
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one-phat/two" |-> Right []
      , expect $ RemoveDirectory "/a/b/one-phat/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one/three" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one-phat/three" |-> Right []
      , expect $ RemoveDirectory "/a/b/one-phat/three" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/Uno.mp3" "/a/b/one/Uno.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst = FirstDirs
      , optOrder = OrderTime
      }

testOrderTimeScript :: TestTree
testOrderTimeScript = testCase "OrderTimeScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ PutStrLn "mv /a/b/one /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
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
      , expect $ PutStrLn "mkdir /a/b/one/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []
      , expect $ PutStrLn "rmdir /a/b/one-phat/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ PutStrLn "rmdir /a/b/one-phat/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "rmdir /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst  = FirstDirs
      , optOrder  = OrderTime
      , optScript = True
      }

------------------------------------------------------------------------------

testOrderTimeReverse :: TestTree
testOrderTimeReverse = testCase "OrderTimeReverse" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one" |-> Right ()
      , expect $ Sync |-> ()
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
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one-phat/three" |-> Right []
      , expect $ RemoveDirectory "/a/b/one-phat/three" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one-phat/two" |-> Right []
      , expect $ RemoveDirectory "/a/b/one-phat/two" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/Uno.mp3" "/a/b/one/Uno.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst   = FirstDirs
      , optOrder   = OrderTime
      , optReverse = True
      }

testOrderTimeReverseScript :: TestTree
testOrderTimeReverseScript = testCase "OrderTimeReverseScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ PutStrLn "mv /a/b/one /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
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
      , expect $ PutStrLn "mkdir /a/b/one/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/three" |-> Right []
      , expect $ PutStrLn "rmdir /a/b/one-phat/three" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one/two" |-> Right []
      , expect $ PutStrLn "rmdir /a/b/one-phat/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "rmdir /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst   = FirstDirs
      , optOrder   = OrderTime
      , optReverse = True
      , optScript  = True
      }

------------------------------------------------------------------------------

testOrderRandom :: TestTree
testOrderRandom = testCase "OrderRandom" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one" |-> Right ()
      , expect $ Sync |-> ()
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
              , expect $ Sync |-> ()
              , expect $ ListDirectory "/a/b/one-phat/d1" |-> Right []
              , expect $ RemoveDirectory "/a/b/one-phat/d1" |-> Right ()
              , expect $ Sync |-> ()
              ]
          , inSequence
              [ expect $ CreateDirectory "/a/b/one/d2" |-> Right ()
              , expect $ Sync |-> ()
              , expect $ ListDirectory "/a/b/one-phat/d2" |-> Right []
              , expect $ RemoveDirectory "/a/b/one-phat/d2" |-> Right ()
              , expect $ Sync |-> ()
              ]
          , inSequence
              [ expect $ CreateDirectory "/a/b/one/d3" |-> Right ()
              , expect $ Sync |-> ()
              , expect $ ListDirectory "/a/b/one-phat/d3" |-> Right []
              , expect $ RemoveDirectory "/a/b/one-phat/d3" |-> Right ()
              , expect $ Sync |-> ()
              ]
          ]
      , inAnyOrder
          [ inSequence
              [ expect $
                  RenameFile "/a/b/one-phat/x.mp3" "/a/b/one/x.mp3" |->
                    Right ()
              , expect $ Sync |-> ()
              ]
          , inSequence
              [ expect $
                  RenameFile "/a/b/one-phat/y.mp3" "/a/b/one/y.mp3" |->
                    Right ()
              , expect $ Sync |-> ()
              ]
          , inSequence
              [ expect $
                  RenameFile "/a/b/one-phat/z.mp3" "/a/b/one/z.mp3" |->
                    Right ()
              , expect $ Sync |-> ()
              ]
          ]
      , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst = FirstDirs
      , optOrder = OrderRandom
      }

testOrderRandomScript :: TestTree
testOrderRandomScript = testCase "OrderRandomScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ PutStrLn "mv /a/b/one /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
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
      , inAnyOrder
          [ inSequence
              [ expect $ PutStrLn "mkdir /a/b/one/d1" |-> ()
              , expect $ PutStrLn "sync" |-> ()
              , expect $ ListDirectory "/a/b/one/d1" |-> Right []
              , expect $ PutStrLn "rmdir /a/b/one-phat/d1" |-> ()
              , expect $ PutStrLn "sync" |-> ()
              ]
          , inSequence
              [ expect $ PutStrLn "mkdir /a/b/one/d2" |-> ()
              , expect $ PutStrLn "sync" |-> ()
              , expect $ ListDirectory "/a/b/one/d2" |-> Right []
              , expect $ PutStrLn "rmdir /a/b/one-phat/d2" |-> ()
              , expect $ PutStrLn "sync" |-> ()
              ]
          , inSequence
              [ expect $ PutStrLn "mkdir /a/b/one/d3" |-> ()
              , expect $ PutStrLn "sync" |-> ()
              , expect $ ListDirectory "/a/b/one/d3" |-> Right []
              , expect $ PutStrLn "rmdir /a/b/one-phat/d3" |-> ()
              , expect $ PutStrLn "sync" |-> ()
              ]
          ]
      , inAnyOrder
          [ inSequence
              [ expect $ PutStrLn "mv /a/b/one-phat/x.mp3 /a/b/one/x.mp3" |->
                  ()
              , expect $ PutStrLn "sync" |-> ()
              ]
          , inSequence
              [ expect $ PutStrLn "mv /a/b/one-phat/y.mp3 /a/b/one/y.mp3" |->
                  ()
              , expect $ PutStrLn "sync" |-> ()
              ]
          , inSequence
              [ expect $ PutStrLn "mv /a/b/one-phat/z.mp3 /a/b/one/z.mp3" |->
                  ()
              , expect $ PutStrLn "sync" |-> ()
              ]
          ]
      , expect $ PutStrLn "rmdir /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optFirst  = FirstDirs
      , optOrder  = OrderRandom
      , optScript = True
      }

------------------------------------------------------------------------------

testVerbose :: TestTree
testVerbose = testCase "Verbose" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ PutStrLn "one" |-> ()
      , expect $ RenameDirectory "/a/b/one" "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ ListDirectory "/a/b/one-phat" |->
          Right ["Uno.mp3", "dos.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one-phat/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one-phat/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one-phat/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "one/Uno.mp3" |-> ()
      , expect $ RenameFile "/a/b/one-phat/Uno.mp3" "/a/b/one/Uno.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ PutStrLn "one/dos.mp3" |-> ()
      , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ PutStrLn "one/tres.mp3" |-> ()
      , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optVerbose = True
      }

testVerboseScript :: TestTree
testVerboseScript = testCase "VerboseScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right False
      , expect $ PutStrLn "echo one" |-> ()
      , expect $ PutStrLn "mv /a/b/one /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["Uno.mp3", "dos.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "echo one/Uno.mp3" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "echo one/dos.mp3" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "echo one/tres.mp3" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "rmdir /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optScript  = True
      , optVerbose = True
      }

------------------------------------------------------------------------------

testMultipleTargets :: TestTree
testMultipleTargets = testCase "MultipleTargets" . runMockT $ do
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
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/one" |-> Right ()
      , expect $ Sync |-> ()
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
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/dos.mp3" "/a/b/one/dos.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/one-phat/tres.mp3" "/a/b/one/tres.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RemoveDirectory "/a/b/one-phat" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ RenameDirectory "/a/b/two" "/a/b/two-phat" |-> Right ()
      , expect $ Sync |-> ()
      , expect $ CreateDirectory "/a/b/two" |-> Right ()
      , expect $ Sync |-> ()
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
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/two-phat/ni.mp3" "/a/b/two/ni.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RenameFile "/a/b/two-phat/san.mp3" "/a/b/two/san.mp3" |->
          Right ()
      , expect $ Sync |-> ()
      , expect $ RemoveDirectory "/a/b/two-phat" |-> Right ()
      , expect $ Sync |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optTargets = NonEmpty.fromList ["one", "two"]
      }

testMultipleTargetsScript :: TestTree
testMultipleTargetsScript = testCase "MultipleTargetsScript" . runMockT $ do
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
      , expect $ PutStrLn "mv /a/b/one /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/one" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/one" |->
          Right ["Uno.mp3", "dos.mp3", "tres.mp3"]
      , expect $ GetFileStatus "/a/b/one/Uno.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ GetFileStatus "/a/b/one/dos.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/one/tres.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ PutStrLn "mv /a/b/one-phat/Uno.mp3 /a/b/one/Uno.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/dos.mp3 /a/b/one/dos.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/one-phat/tres.mp3 /a/b/one/tres.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "rmdir /a/b/one-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/two /a/b/two-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mkdir /a/b/two" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ ListDirectory "/a/b/two" |->
          Right ["san.mp3", "ni.mp3", "ichi.mp3"]
      , expect $ GetFileStatus "/a/b/two/san.mp3" |->
          Right (FileStatus 11 False 3000)
      , expect $ GetFileStatus "/a/b/two/ni.mp3" |->
          Right (FileStatus 11 False 2000)
      , expect $ GetFileStatus "/a/b/two/ichi.mp3" |->
          Right (FileStatus 11 False 1000)
      , expect $ PutStrLn "mv /a/b/two-phat/ichi.mp3 /a/b/two/ichi.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/two-phat/ni.mp3 /a/b/two/ni.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "mv /a/b/two-phat/san.mp3 /a/b/two/san.mp3" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      , expect $ PutStrLn "rmdir /a/b/two-phat" |-> ()
      , expect $ PutStrLn "sync" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optScript  = True
      , optTargets = NonEmpty.fromList ["one", "two"]
      }

------------------------------------------------------------------------------

testTargetNotFound :: TestTree
testTargetNotFound = testCase "TargetNotFound" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |->
          Left (userError "file not found: one")
      ]
    assertError "user error (file not found: one)" <=< Error.run $
      run defaultOptions

------------------------------------------------------------------------------

testPhatTarget :: TestTree
testPhatTarget = testCase "PhatTarget" . runMockT $
    assertError "-phat directory: one-phat" <=< Error.run $
      run defaultOptions
        { optTargets = NonEmpty.fromList ["one-phat"]
        }

------------------------------------------------------------------------------

testTargetNotDirectory :: TestTree
testTargetNotDirectory = testCase "TargetNotDirectory" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 False 10000)
      ]
    assertError "not a directory: one" <=< Error.run $ run defaultOptions

------------------------------------------------------------------------------

testTargetMountPoint :: TestTree
testTargetMountPoint = testCase "TargetMountPoint" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/one"
      , expect $ GetFileStatus "/a/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a" |-> Right (FileStatus 10 True 100)
      ]
    assertError "mount point: one" <=< Error.run $ run defaultOptions

------------------------------------------------------------------------------

testTargetPhatExists :: TestTree
testTargetPhatExists = testCase "TargetPhatExists" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "one" |-> Right "/a/b/one"
      , expect $ GetFileStatus "/a/b/one" |->
          Right (FileStatus 11 True 10000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/one-phat" |-> Right True
      ]
    assertError "already exists: one-phat" <=< Error.run $ run defaultOptions

------------------------------------------------------------------------------

testLarge :: TestTree
testLarge = testCase "Large" . runMockT $ do
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
      , expect $ CreateDirectory "/a/b/c" |-> Right ()
      , expect $ ListDirectory "/a/b/c-phat" |-> Right ["two", "one"]
      , expect $ GetFileStatus "/a/b/c-phat/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/c-phat/one" |->
          Right (FileStatus 11 True 1000)
      , expect $ CreateDirectory "/a/b/c/one" |-> Right ()
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
      , expect $ RenameFile "/a/b/c-phat/one/12.mp3" "/a/b/c/one/12.mp3" |->
          Right ()
      , expect $ RenameFile "/a/b/c-phat/one/13.mp3" "/a/b/c/one/13.mp3" |->
          Right ()
      , expect $ RenameFile "/a/b/c-phat/one/14.mp3" "/a/b/c/one/14.mp3" |->
          Right ()
      , expect $ CreateDirectory "/a/b/c/one/eight" |-> Right ()
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
      , expect $
          RenameFile
            "/a/b/c-phat/one/eight/82.mp3"
            "/a/b/c/one/eight/82.mp3"
            |-> Right ()
      , expect $
          RenameFile
            "/a/b/c-phat/one/eight/83.mp3"
            "/a/b/c/one/eight/83.mp3"
            |-> Right ()
      , expect $ RemoveDirectory "/a/b/c-phat/one/eight" |-> Right ()
      , expect $ CreateDirectory "/a/b/c/one/six" |-> Right ()
      , expect $ ListDirectory "/a/b/c-phat/one/six" |-> Right []
      , expect $ RemoveDirectory "/a/b/c-phat/one/six" |-> Right ()
      , expect $ RemoveDirectory "/a/b/c-phat/one" |-> Right ()
      , expect $ CreateDirectory "/a/b/c/two" |-> Right ()
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
      , expect $ RenameFile "/a/b/c-phat/two/22.mp3" "/a/b/c/two/22.mp3" |->
          Right ()
      , expect $ RenameFile "/a/b/c-phat/two/23.mp3" "/a/b/c/two/23.mp3" |->
          Right ()
      , expect $ CreateDirectory "/a/b/c/two/five" |-> Right ()
      , expect $ ListDirectory "/a/b/c-phat/two/five" |-> Right ["51.mp3"]
      , expect $ GetFileStatus "/a/b/c-phat/two/five/51.mp3" |->
          Right (FileStatus 11 False 5100)
      , expect $
          RenameFile "/a/b/c-phat/two/five/51.mp3" "/a/b/c/two/five/51.mp3"
          |-> Right ()
      , expect $ RemoveDirectory "/a/b/c-phat/two/five" |-> Right ()
      , expect $ CreateDirectory "/a/b/c/two/seven" |-> Right ()
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
      , expect $
          RenameFile
            "/a/b/c-phat/two/seven/72.mp3"
            "/a/b/c/two/seven/72.mp3"
            |-> Right ()
      , expect $ RemoveDirectory "/a/b/c-phat/two/seven" |-> Right ()
      , expect $ RemoveDirectory "/a/b/c-phat/two" |-> Right ()
      , expect $ RemoveDirectory "/a/b/c-phat" |-> Right ()
      , expect $ RenameDirectory "/a/b/d" "/a/b/d-phat" |-> Right ()
      , expect $ CreateDirectory "/a/b/d" |-> Right ()
      , expect $ ListDirectory "/a/b/d-phat" |-> Right ["three"]
      , expect $ GetFileStatus "/a/b/d-phat/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ CreateDirectory "/a/b/d/three" |-> Right ()
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
      , expect $
          RenameFile "/a/b/d-phat/three/32.mp3" "/a/b/d/three/32.mp3" |->
            Right ()
      , expect $
          RenameFile "/a/b/d-phat/three/33.mp3" "/a/b/d/three/33.mp3" |->
            Right ()
      , expect $
          RenameFile "/a/b/d-phat/three/34.mp3" "/a/b/d/three/34.mp3" |->
            Right ()
      , expect $ CreateDirectory "/a/b/d/three/four" |-> Right ()
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
      , expect $
          RenameFile
            "/a/b/d-phat/three/four/42.mp3"
            "/a/b/d/three/four/42.mp3"
          |-> Right ()
      , expect $ RemoveDirectory "/a/b/d-phat/three/four" |-> Right ()
      , expect $ CreateDirectory "/a/b/d/three/nine" |-> Right ()
      , expect $ ListDirectory "/a/b/d-phat/three/nine" |-> Right ["ten"]
      , expect $ GetFileStatus "/a/b/d-phat/three/nine/ten" |->
          Right (FileStatus 11 True 10000)
      , expect $ CreateDirectory "/a/b/d/three/nine/ten" |-> Right ()
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
      , expect $
          RenameFile
            "/a/b/d-phat/three/nine/ten/102.mp3"
            "/a/b/d/three/nine/ten/102.mp3"
            |-> Right ()
      , expect $ RemoveDirectory "/a/b/d-phat/three/nine/ten" |-> Right ()
      , expect $ RemoveDirectory "/a/b/d-phat/three/nine" |-> Right ()
      , expect $ RemoveDirectory "/a/b/d-phat/three" |-> Right ()
      , expect $ RemoveDirectory "/a/b/d-phat" |-> Right ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optSync    = False
      , optTargets = NonEmpty.fromList ["c", "d"]
      }

testLargeScript :: TestTree
testLargeScript = testCase "LargeScript" . runMockT $ do
    inSequence
      [ expect $ MakeAbsolute "c" |-> Right "/a/b/c"
      , expect $ GetFileStatus "/a/b/c" |-> Right (FileStatus 11 True 1000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/c-phat" |-> Right False
      , expect $ MakeAbsolute "d" |-> Right "/a/b/d"
      , expect $ GetFileStatus "/a/b/d" |-> Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b" |-> Right (FileStatus 11 True 100)
      , expect $ DoesPathExist "/a/b/d-phat" |-> Right False
      , expect $ PutStrLn "mv /a/b/c /a/b/c-phat" |-> ()
      , expect $ PutStrLn "mkdir /a/b/c" |-> ()
      , expect $ ListDirectory "/a/b/c" |-> Right ["two", "one"]
      , expect $ GetFileStatus "/a/b/c/two" |->
          Right (FileStatus 11 True 2000)
      , expect $ GetFileStatus "/a/b/c/one" |->
          Right (FileStatus 11 True 1000)
      , expect $ PutStrLn "mkdir /a/b/c/one" |-> ()
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
      , expect $ PutStrLn "mv /a/b/c-phat/one/11.mp3 /a/b/c/one/11.mp3" |-> ()
      , expect $ PutStrLn "mv /a/b/c-phat/one/12.mp3 /a/b/c/one/12.mp3" |-> ()
      , expect $ PutStrLn "mv /a/b/c-phat/one/13.mp3 /a/b/c/one/13.mp3" |-> ()
      , expect $ PutStrLn "mv /a/b/c-phat/one/14.mp3 /a/b/c/one/14.mp3" |-> ()
      , expect $ PutStrLn "mkdir /a/b/c/one/eight" |-> ()
      , expect $ ListDirectory "/a/b/c/one/eight" |->
          Right ["83.mp3", "81.mp3", "82.mp3"]
      , expect $ GetFileStatus "/a/b/c/one/eight/83.mp3" |->
          Right (FileStatus 11 False 8300)
      , expect $ GetFileStatus "/a/b/c/one/eight/81.mp3" |->
          Right (FileStatus 11 False 8100)
      , expect $ GetFileStatus "/a/b/c/one/eight/82.mp3" |->
          Right (FileStatus 11 False 8200)
      , expect $
          PutStrLn
            "mv /a/b/c-phat/one/eight/81.mp3 /a/b/c/one/eight/81.mp3"
            |-> ()
      , expect $
          PutStrLn
            "mv /a/b/c-phat/one/eight/82.mp3 /a/b/c/one/eight/82.mp3"
            |-> ()
      , expect $
          PutStrLn
            "mv /a/b/c-phat/one/eight/83.mp3 /a/b/c/one/eight/83.mp3"
            |-> ()
      , expect $ PutStrLn "rmdir /a/b/c-phat/one/eight" |-> ()
      , expect $ PutStrLn "mkdir /a/b/c/one/six" |-> ()
      , expect $ ListDirectory "/a/b/c/one/six" |-> Right []
      , expect $ PutStrLn "rmdir /a/b/c-phat/one/six" |-> ()
      , expect $ PutStrLn "rmdir /a/b/c-phat/one" |-> ()
      , expect $ PutStrLn "mkdir /a/b/c/two" |-> ()
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
      , expect $ PutStrLn "mv /a/b/c-phat/two/21.mp3 /a/b/c/two/21.mp3" |-> ()
      , expect $ PutStrLn "mv /a/b/c-phat/two/22.mp3 /a/b/c/two/22.mp3" |-> ()
      , expect $ PutStrLn "mv /a/b/c-phat/two/23.mp3 /a/b/c/two/23.mp3" |-> ()
      , expect $ PutStrLn "mkdir /a/b/c/two/five" |-> ()
      , expect $ ListDirectory "/a/b/c/two/five" |-> Right ["51.mp3"]
      , expect $ GetFileStatus "/a/b/c/two/five/51.mp3" |->
          Right (FileStatus 11 False 5100)
      , expect $
          PutStrLn "mv /a/b/c-phat/two/five/51.mp3 /a/b/c/two/five/51.mp3" |->
            ()
      , expect $ PutStrLn "rmdir /a/b/c-phat/two/five" |-> ()
      , expect $ PutStrLn "mkdir /a/b/c/two/seven" |-> ()
      , expect $ ListDirectory "/a/b/c/two/seven" |->
          Right ["72.mp3", "71.mp3"]
      , expect $ GetFileStatus "/a/b/c/two/seven/72.mp3" |->
          Right (FileStatus 11 False 7200)
      , expect $ GetFileStatus "/a/b/c/two/seven/71.mp3" |->
          Right (FileStatus 11 False 7100)
      , expect $
          PutStrLn
            "mv /a/b/c-phat/two/seven/71.mp3 /a/b/c/two/seven/71.mp3"
            |-> ()
      , expect $
          PutStrLn
            "mv /a/b/c-phat/two/seven/72.mp3 /a/b/c/two/seven/72.mp3"
            |-> ()
      , expect $ PutStrLn "rmdir /a/b/c-phat/two/seven" |-> ()
      , expect $ PutStrLn "rmdir /a/b/c-phat/two" |-> ()
      , expect $ PutStrLn "rmdir /a/b/c-phat" |-> ()
      , expect $ PutStrLn "mv /a/b/d /a/b/d-phat" |-> ()
      , expect $ PutStrLn "mkdir /a/b/d" |-> ()
      , expect $ ListDirectory "/a/b/d" |-> Right ["three"]
      , expect $ GetFileStatus "/a/b/d/three" |->
          Right (FileStatus 11 True 3000)
      , expect $ PutStrLn "mkdir /a/b/d/three" |-> ()
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
      , expect $
          PutStrLn "mv /a/b/d-phat/three/31.mp3 /a/b/d/three/31.mp3" |-> ()
      , expect $
          PutStrLn "mv /a/b/d-phat/three/32.mp3 /a/b/d/three/32.mp3" |-> ()
      , expect $
          PutStrLn "mv /a/b/d-phat/three/33.mp3 /a/b/d/three/33.mp3" |-> ()
      , expect $
          PutStrLn "mv /a/b/d-phat/three/34.mp3 /a/b/d/three/34.mp3" |-> ()
      , expect $ PutStrLn "mkdir /a/b/d/three/four" |-> ()
      , expect $ ListDirectory "/a/b/d/three/four" |->
          Right ["41.mp3", "42.mp3"]
      , expect $ GetFileStatus "/a/b/d/three/four/41.mp3" |->
          Right (FileStatus 11 False 4100)
      , expect $ GetFileStatus "/a/b/d/three/four/42.mp3" |->
          Right (FileStatus 11 False 4200)
      , expect $
          PutStrLn
            "mv /a/b/d-phat/three/four/41.mp3 /a/b/d/three/four/41.mp3"
            |-> ()
      , expect $
          PutStrLn
            "mv /a/b/d-phat/three/four/42.mp3 /a/b/d/three/four/42.mp3"
            |-> ()
      , expect $ PutStrLn "rmdir /a/b/d-phat/three/four" |-> ()
      , expect $ PutStrLn "mkdir /a/b/d/three/nine" |-> ()
      , expect $ ListDirectory "/a/b/d/three/nine" |-> Right ["ten"]
      , expect $ GetFileStatus "/a/b/d/three/nine/ten" |->
          Right (FileStatus 11 True 10000)
      , expect $ PutStrLn "mkdir /a/b/d/three/nine/ten" |-> ()
      , expect $ ListDirectory "/a/b/d/three/nine/ten" |->
          Right ["102.mp3", "101.mp3"]
      , expect $ GetFileStatus "/a/b/d/three/nine/ten/102.mp3" |->
          Right (FileStatus 11 False 10200)
      , expect $ GetFileStatus "/a/b/d/three/nine/ten/101.mp3" |->
          Right (FileStatus 11 False 10100)
      , expect $
          PutStrLn
            "mv /a/b/d-phat/three/nine/ten/101.mp3 /a/b/d/three/nine/ten/101.mp3"
            |-> ()
      , expect $
          PutStrLn
            "mv /a/b/d-phat/three/nine/ten/102.mp3 /a/b/d/three/nine/ten/102.mp3"
            |-> ()
      , expect $ PutStrLn "rmdir /a/b/d-phat/three/nine/ten" |-> ()
      , expect $ PutStrLn "rmdir /a/b/d-phat/three/nine" |-> ()
      , expect $ PutStrLn "rmdir /a/b/d-phat/three" |-> ()
      , expect $ PutStrLn "rmdir /a/b/d-phat" |-> ()
      ]
    assertSuccess <=< Error.run $ run defaultOptions
      { optSync    = False
      , optScript  = True
      , optTargets = NonEmpty.fromList ["c", "d"]
      }

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

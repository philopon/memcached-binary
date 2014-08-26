{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Network
import System.Process

import Control.Exception
import Control.Concurrent
import Control.Monad

import Data.Default.Class
import Data.Maybe
import Data.Word
import Data.Typeable
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

import Database.Memcached.Binary.Types.Exception
import Database.Memcached.Binary.IO (Connection, withConnection)
import qualified Database.Memcached.Binary.IO    as McIO
import qualified Database.Memcached.Binary.Maybe as McMaybe

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

startMemcached :: IO ProcessHandle
startMemcached = do
    h <- spawnProcess "memcached" []
    wait (100 :: Int)
    return h
  where
    wait 0 = fail "cannot start server"
    wait i = handle (\(_ ::SomeException) -> threadDelay 100000 >> wait (i-1)) $
        void $ connectTo "localhost" $ PortNumber 11211

precond :: Connection -> IO ()
precond c = do
    McIO.flushAll c
    void $ McIO.set 0 0 "foo" "foovalue"   c
    void $ McIO.set 1 0 "bar" "1234567890" c

testMc :: TestName -> (Connection -> IO ()) -> Test
testMc title m = testCase title $ withConnection def (\c -> precond c >> m c)

newtype ByPassException = ByPassException String deriving (Typeable)
instance Show ByPassException where
    show (ByPassException s) = s

instance Exception ByPassException

assertException :: Word16 -> S.ByteString -> IO a -> IO ()
assertException ex msg m =
    (m >> throwIO (ByPassException "exception not occured.")) `catch`
    (\e -> case fromException e :: Maybe MemcachedException of
        Nothing -> assertFn e
        Just e'@(MemcachedException i _) -> unless (i == ex) (assertFn e'))
  where
    assertFn e = assertFailure $ unlines
        [ "not expected exception occured:"
        , "expected: " ++ show (MemcachedException ex msg)
        , "occured:  " ++ show e
        ]

testGet :: Test
testGet = testGroup "get"
    [ testGroup "IO module"
        [ testGroup "get"
            [ doTest "foo" (0, "foovalue")   McIO.get
            , doTest "bar" (1, "1234567890") McIO.get
            , doTestException "notexist"     McIO.get
            ]
        , testGroup "get_"
            [ doTest "foo" "foovalue"    McIO.get_
            , doTest "bar" "1234567890"  McIO.get_
            , doTestException "notexist" McIO.get_
            ]
        ]
    , testGroup "Maybe module"
        [ testGroup "get"
            [ doTest "foo"      (Just (0, "foovalue"))   McMaybe.get
            , doTest "bar"      (Just (1, "1234567890")) McMaybe.get
            , doTest "notexist" Nothing                  McMaybe.get
            ]
         , testGroup "get_"
            [ doTest "foo"      (Just "foovalue")   McMaybe.get_
            , doTest "bar"      (Just "1234567890") McMaybe.get_
            , doTest "notexist" Nothing             McMaybe.get_
            ]
        ]
    ]
  where
    doTest key ex fn = testMc (S.unpack key) $ \c -> do
        v <- fn key c
        v @?= ex
    doTestException key fn = testMc (S.unpack key) $ \c ->
        assertException 1 "Not found" $ fn key c

testSetAddReplace :: Test
testSetAddReplace = testGroup "set/add/replace"
    [ testGroup "set"
        [ testMc "set foo to foomod" $ \c -> do
            McIO.set 100 0 "foo" "foomod" c
            v <- McIO.get "foo" c
            v @?= (100, "foomod")
        , testMc "set notexist to exist" $ \c -> do
            McIO.set 100 0 "notexist" "exist" c
            v <- McIO.get "notexist" c
            v @?= (100, "exist")
        ]
    , testGroup "add"
        [ testMc "add foo to foomod" $ \c ->
            assertException 2 "Data exists for key." $
                McIO.add 100 0 "foo" "foomod" c
        , testMc "add notexist to exist" $ \c -> do
            McIO.add 100 0 "notexist" "exist" c
            v <- McIO.get "notexist" c
            v @?= (100, "exist")
        ]
    , testGroup "replace"
        [ testMc "set foo to foomod" $ \c -> do
            McIO.replace 100 0 "foo" "foomod" c
            v <- McIO.get "foo" c
            v @?= (100, "foomod")
        , testMc "set notexist to exist" $ \c ->
            assertException 1 "Not found" $
                McIO.replace 100 0 "notexist" "exist" c
        ]
    ]

testDelete :: Test
testDelete = testGroup "delete"
    [ testGroup "IO module"
        [ testMc "foo" $ \c -> do
            McIO.delete "foo" c
            r <- McMaybe.get "foo" c
            r @?= Nothing
        , testMc "notexist" $ \c ->
            assertException 1 "Not found" $ McIO.delete "notexist" c
        ]
    , testGroup "Maybe module"
        [ testMc "foo" $ \c -> do
            b <- McMaybe.delete "foo" c
            r <- McMaybe.get "foo" c
            b @?= True
            r @?= Nothing
        , testMc "notexist" $ \c -> do
            b <- McMaybe.delete "notexist" c
            r <- McMaybe.get    "notexist" c
            b @?= False
            r @?= Nothing
        ]
    ]

testIncrDecr :: Test
testIncrDecr = testGroup "increment/decrement"
    [ testGroup "IO module"
        [ testGroup "increment"
            [ testMc "foo" $ \c ->
                assertException 6 "Non-numeric server-side value for incr or decr" $
                    McIO.increment 0 "foo" 10 10 c
            , testMc "bar" $ \c -> do
                a <- McIO.increment 0 "bar" 10 10 c
                b <- McIO.get_ "bar" c
                a @?= 1234567900
                b @?= "1234567900"
            , testMc "notexist" $ \c -> do
                a <- McIO.increment 0 "notexist" 10 10 c
                b <- McIO.get_ "notexist" c
                a @?= 10
                b @?= "10"
            ]
        , testGroup "decrement"
            [ testMc "foo" $ \c ->
                assertException 6 "Non-numeric server-side value for incr or decr" $
                    McIO.decrement 0 "foo" 10 10 c
            , testMc "bar" $ \c -> do
                a <- McIO.decrement 0 "bar" 10 10 c
                b <- McIO.get_ "bar" c
                a @?= 1234567880
                b @?= "1234567880"
            , testMc "notexist" $ \c -> do
                a <- McIO.decrement 0 "notexist" 10 10 c
                b <- McIO.get_ "notexist" c
                a @?= 10
                b @?= "10"
            ]
        ]
    , testGroup "Maybe module"
        [ testGroup "increment"
            [ testMc "foo" $ \c -> do
                r <- McMaybe.increment 0 "foo" 10 10 c
                b <- McIO.get_ "foo" c
                r @?= Nothing
                b @?= "foovalue"
            , testMc "bar" $ \c -> do
                a <- McMaybe.increment 0 "bar" 10 10 c
                b <- McIO.get_ "bar" c
                a @?= Just 1234567900
                b @?= "1234567900"
            , testMc "notexist" $ \c -> do
                a <- McMaybe.increment 0 "notexist" 10 10 c
                b <- McIO.get_ "notexist" c
                a @?= Just 10
                b @?= "10"
            ]
        , testGroup "decrement'"
            [ testMc "foo" $ \c -> do
                r <- McMaybe.decrement 0 "foo" 10 10 c
                b <- McIO.get_ "foo" c
                r @?= Nothing
                b @?= "foovalue"
            , testMc "bar" $ \c -> do
                a <- McMaybe.decrement 0 "bar" 10 10 c
                b <- McIO.get_ "bar" c
                a @?= Just 1234567880
                b @?= "1234567880"
            , testMc "notexist" $ \c -> do
                a <- McMaybe.decrement 0 "notexist" 10 10 c
                b <- McIO.get_ "notexist" c
                a @?= Just 10
                b @?= "10"
            ]
        ]
    ]

testFlush :: Test
testFlush = testGroup "flush"
    [ testGroup "IO module"
        [ testMc "flushAll" $ \c -> do
            McIO.flushAll c
            a <- McMaybe.get "foo" c
            a @?= Nothing
        ]
    , testGroup "Maybe module"
        [ testMc "flushAll" $ \c -> do
            r <- McMaybe.flushAll c
            a <- McMaybe.get "foo" c
            r @?= True
            a @?= Nothing
        ]
    ]

testVersion :: Test
testVersion = testGroup "version"
    [ testMc "IO module" $ \c -> do
        v <- McIO.version c
        assertBool (show v ++ " is not version like.") $ isVersionLike v
    , testMc "Maybe module" $ \c -> do
        Just v <- McMaybe.version c
        assertBool (show v ++ " is not version like.") $ isVersionLike v
    ]
  where
    isVersionLike s0 = isJust $ do
        (_, s1) <- S.readInt s0
        unless (S.head s1 == '.') Nothing
        (_, s2) <- S.readInt (S.tail s1)
        unless (S.head s2 == '.') Nothing
        (_, s3) <- S.readInt (S.tail s2)
        unless (S.null s3) Nothing

testNoOp :: Test
testNoOp = testGroup "noOp"
    [ testMc "IO module" McIO.noOp
    , testMc "Maybe module" $ \c -> do
        b <- McMaybe.noOp c
        b @?= True
    ]

testAppendPrepend :: Test
testAppendPrepend = testGroup "append/prepend"
    [ testGroup "IO module"
        [ testGroup "append"
            [ testMc "foo" $ \c -> do
                McIO.append "foo" "!!" c
                a <- McIO.get_ "foo" c
                a @?= "foovalue!!"
            , testMc "bar" $ \c -> do
                McIO.append "bar" "!!" c
                a <- McIO.get_ "bar" c
                a @?= "1234567890!!"
            , testMc "notexist" $ \c ->
                assertException 5 "Not stored." $ McIO.append "notexist" "!!" c
            ]
        , testGroup "prepend"
            [ testMc "foo" $ \c -> do
                McIO.prepend "foo" "!!" c
                a <- McIO.get_ "foo" c
                a @?= "!!foovalue"
            , testMc "bar" $ \c -> do
                McIO.prepend "bar" "!!" c
                a <- McIO.get_ "bar" c
                a @?= "!!1234567890"
            , testMc "notexist" $ \c ->
                assertException 5 "Not stored." $ McIO.prepend "notexist" "!!" c
            ]
        ]
    , testGroup "maybe module"
        [ testGroup "append'"
            [ testMc "foo" $ \c -> do
                b <- McMaybe.append "foo" "!!" c
                a <- McIO.get_ "foo" c
                b @?= True
                a @?= "foovalue!!"
            , testMc "bar" $ \c -> do
                b <- McMaybe.append "bar" "!!" c
                a <- McIO.get_ "bar" c
                b @?= True
                a @?= "1234567890!!"
            , testMc "notexist" $ \c -> do
                b <- McMaybe.append "notexist" "!!" c
                b @?= False
            ]
        , testGroup "prepend'"
            [ testMc "foo" $ \c -> do
                b <- McMaybe.prepend "foo" "!!" c
                a <- McIO.get_ "foo" c
                b @?= True
                a @?= "!!foovalue"
            , testMc "bar" $ \c -> do
                b <- McMaybe.prepend "bar" "!!" c
                a <- McIO.get_ "bar" c
                b @?= True
                a @?= "!!1234567890"
            , testMc "notexist" $ \c -> do
                b <- McMaybe.prepend "notexist" "!!" c
                b @?= False
            ]
        ]
    ]

testTouchGAT :: Test
testTouchGAT = testGroup "touch/GAT"
    [ testGroup "IO module"
        [ testGroup "touch"
            [ testMc "foo" $ \c -> do
                a <- McMaybe.get_ "foo" c
                McIO.touch 1 "foo" c
                threadDelay 1100000
                b <- McMaybe.get_ "foo" c
                a @?= (Just "foovalue")
                b @?= Nothing
            , testMc "notexist" $ \c ->
                assertException 1 "Not found" $ McIO.touch 1 "notexist" c
            ]
        , testGroup "getAndTouch"
            [ testMc "foo" $ \c -> do
                x <- McMaybe.get "foo" c
                y <- McIO.getAndTouch 1 "foo" c
                threadDelay 1100000
                z <- McMaybe.get "foo" c
                x @?= Just (0, "foovalue")
                y @?= (0, "foovalue")
                z @?= Nothing
            , testMc "notexist" $ \c ->
                assertException 1 "Not found" $ McIO.getAndTouch 1 "notexist" c
            ]
        , testGroup "getAndTouch_"
            [ testMc "foo" $ \c -> do
                x <- McMaybe.get_ "foo" c
                y <- McIO.getAndTouch_ 1 "foo" c
                threadDelay 1100000
                z <- McMaybe.get_ "foo" c
                x @?= Just "foovalue"
                y @?=      "foovalue"
                z @?= Nothing
            , testMc "notexist" $ \c ->
                assertException 1 "Not found" $ McIO.getAndTouch_ 1 "notexist" c
            ]
        ]
    , testGroup "Maybe module"
        [ testGroup "touch"
            [ testMc "foo" $ \c -> do
                a <- McMaybe.get_ "foo" c
                r <- McMaybe.touch 1 "foo" c
                threadDelay 1100000
                b <- McMaybe.get_ "foo" c
                a @?= (Just "foovalue")
                r @?= True
                b @?= Nothing
            , testMc "notexist" $ \c -> do
                r <- McMaybe.touch 1 "notexist" c
                a <- McMaybe.get "notexist" c
                r @?= False
                a @?= Nothing
            ]
        , testGroup "getAndTouch'/getMaybeAndTouch"
            [ testMc "foo" $ \c -> do
                a <- McMaybe.get "foo" c
                r <- McMaybe.getAndTouch 1 "foo" c
                threadDelay 1100000
                b <- McMaybe.get_ "foo" c
                a @?= Just (0, "foovalue")
                r @?= Just (0, "foovalue")
                b @?= Nothing
            , testMc "notexist" $ \c -> do
                r <- McMaybe.getAndTouch 1 "notexist" c
                a <- McMaybe.get "notexist" c
                r @?= Nothing
                a @?= Nothing
            ]
        , testGroup "getAndTouch'_/getMaybeAndTouch_"
            [ testMc "foo" $ \c -> do
                a <- McMaybe.get_ "foo" c
                r <- McMaybe.getAndTouch_ 1 "foo" c
                threadDelay 1100000
                b <- McMaybe.get_ "foo" c
                a @?= Just "foovalue"
                r @?= Just "foovalue"
                b @?= Nothing
            , testMc "notexist" $ \c -> do
                r <- McMaybe.getAndTouch_ 1 "notexist" c
                a <- McMaybe.get "notexist" c
                r @?= Nothing
                a @?= Nothing
            ]
        ]
    ]

testModify :: Test
testModify = testGroup "modify"
    [ testMc "reverse foo" $ \c -> do
        r <- McIO.modify 0 "foo" (\f v -> (f + 100, L.reverse v, v)) c
        a <- McMaybe.get "foo" c
        r @?= "foovalue"
        a @?= Just (100, "eulavoof")
    , testMc "notexist" $ \c ->
        assertException 1 "Not found" $
            McIO.modify 0 "notexist" (\f v -> (f,v,())) c
    ]

testModify_ :: Test
testModify_ = testGroup "modify_"
    [ testMc "reverse foo" $ \c -> do
        McIO.modify_ 0 "foo" (\f v -> (f + 100, L.reverse v)) c
        a <- McMaybe.get "foo" c
        a @?= Just (100, "eulavoof")
    , testMc "notexist" $ \c ->
        assertException 1 "Not found" $
            McIO.modify_ 0 "notexist" (\f v -> (f,v)) c
    ]

main :: IO ()
main = bracket startMemcached terminateProcess $ \_ -> defaultMain
    [ testGet
    , testSetAddReplace
    , testDelete
    , testIncrDecr
    , testFlush
    , testVersion
    , testNoOp
    , testAppendPrepend
    , testTouchGAT
    , testModify
    , testModify_
    ]

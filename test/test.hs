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
import Data.Version
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

import Database.Memcached.Binary.Types.Exception
import Database.Memcached.Binary.IO (Connection, withConnection)
import qualified Database.Memcached.Binary.IO    as McIO
import qualified Database.Memcached.Binary.Maybe as McMaybe

import Test.Hspec
import Test.HUnit hiding (Test)

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

withConn :: (Connection -> IO a) -> IO a
withConn m = withConnection def $ \c -> precond c >> m c

main :: IO ()
main = bracket startMemcached terminateProcess $ \_ -> hspec $ do
    v <- runIO $ withConn McIO.version
    testGet
    testSetAddReplace
    testDelete
    testIncrDecr v
    testFlush
    testVersion
    testNoOp
    testAppendPrepend
    testTouchGAT v
    testModify

testGet :: Spec
testGet = context "get" $ do
    context "IO module" $ do
        describe "get" $ do
            doTest "foo" (0, "foovalue")   "get" McIO.get
            doTest "bar" (1, "1234567890") "get" McIO.get
            doTestException                "get" McIO.get
        describe "get_" $ do
            doTest "foo" "foovalue"   "get_" McIO.get_
            doTest "bar" "1234567890" "get_" McIO.get_
            doTestException           "get_" McIO.get_
    context "Maybe module" $ do
        describe "get" $ do
            doTest "foo" (Just (0, "foovalue"))   "get" McMaybe.get
            doTest "bar" (Just (1, "1234567890")) "get" McMaybe.get
            doTest "notexist" Nothing             "get" McMaybe.get
        describe "get_" $ do
            doTest "foo" (Just "foovalue")   "get_" McMaybe.get_
            doTest "bar" (Just "1234567890") "get_" McMaybe.get_
            doTest "notexist" Nothing        "get_" McMaybe.get_
  where
    doTest key ex meth fn = it ("return " ++ show ex ++ " when " ++ meth ++ ' ': show key) $ withConn $ \c -> do
        v <- fn key c
        v @?= ex
    doTestException meth fn = it ("throw exception(1) when " ++ meth ++ " notexist") $ withConn $ \c ->
        assertException 1 "Not found" $ fn "notexist" c

testSetAddReplace :: Spec
testSetAddReplace = context "set/add/replace" $ do
    describe "set" $ do
        it "set foo = (100, foomod)" $ withConn $ \c -> do
            McIO.set 100 0 "foo" "foomod" c
            v <- McIO.get "foo" c
            v @?= (100, "foomod")
        it "set notexist = exist" $ withConn $ \c -> do
            McIO.set 100 0 "notexist" "exist" c
            v <- McIO.get "notexist" c
            v @?= (100, "exist")

    describe "add" $ do
        it "throw exception(2) when add exist key" $ withConn $ \c -> do
            assertException 2 "Data exists for key." $
                McIO.add 100 0 "foo" "foomod" c
        it "add notexist = exist" $ withConn $ \c -> do
            McIO.add 100 0 "notexist" "exist" c
            v <- McIO.get "notexist" c
            v @?= (100, "exist")

    describe "replace" $ do
        it "replace foo = foomod" $ withConn $ \c -> do
            McIO.replace 100 0 "foo" "foomod" c
            v <- McIO.get "foo" c
            v @?= (100, "foomod")
        it "throw exception(1) when replace not exist key" $ withConn $ \c -> do
            assertException 1 "Not found" $
                McIO.replace 100 0 "notexist" "exist" c


testDelete :: Spec
testDelete = context "delete" $ do
    context "IO module" $ do
        it "delete foo" $ withConn $ \c -> do
            McIO.delete "foo" c
            r <- McMaybe.get "foo" c
            r @?= Nothing
        it "throw exception(1) when delete notexist" $ withConn $
            assertException 1 "Not found" . McIO.delete "notexist"

    context "Maybe module" $ do
        it "delete foo" $ withConn $ \c -> do
            b <- McMaybe.delete "foo" c
            r <- McMaybe.get "foo" c
            b @?= True
            r @?= Nothing
        it "delete notexist" $ withConn $ \c -> do
            b <- McMaybe.delete "notexist" c
            r <- McMaybe.get    "notexist" c
            b @?= False
            r @?= Nothing

-- https://code.google.com/p/memcached/wiki/ReleaseNotes1417
testIncrDecr :: Version -> Spec
testIncrDecr v = context "increment/decrement" $ do
    context "IO module" $ do
        describe "increment" $ do
            it "throw exception(6) when increment non numeric value" $ withConn $ \c ->
                assertException 6 "Non-numeric server-side value for incr or decr" $
                    McIO.increment 0 "foo" 10 10 c

            it "increment bar" $ withConn $ \c -> do
                a <- McIO.increment 0 "bar" 10 10 c
                b <- McIO.get_ "bar" c
                a @?= 1234567900
                b @?= "1234567900"

            it "set initial value notexist" $ withConn $ \c -> do
                when (v < ev) $ pendingWith msg
                a <- McIO.increment 0 "notexist" 10 10 c
                b <- McIO.get_ "notexist" c
                a @?= 10
                b @?= "10"

        describe "decrement" $ do
            it "throw exception(6) when increment non numeric value" $ withConn $ \c ->
                assertException 6 "Non-numeric server-side value for incr or decr" $
                    McIO.decrement 0 "foo" 10 10 c

            it "decrement bar" $ withConn $ \c -> do
                a <- McIO.decrement 0 "bar" 10 10 c
                b <- McIO.get_ "bar" c
                a @?= 1234567880
                b @?= "1234567880"

            it "set initial value notexist" $ withConn $ \c -> do
                when (v < ev) $ pendingWith msg
                a <- McIO.decrement 0 "notexist" 10 10 c
                b <- McIO.get_ "notexist" c
                a @?= 10
                b @?= "10"

    context "Maybe module" $ do
        describe "increment" $ do
            it "return Nothing when increment non numeric value" $ withConn $ \c -> do
                r <- McMaybe.increment 0 "foo" 10 10 c
                b <- McIO.get_ "foo" c
                r @?= Nothing
                b @?= "foovalue"
            it "increment bar" $ withConn $ \c -> do
                a <- McMaybe.increment 0 "bar" 10 10 c
                b <- McIO.get_ "bar" c
                a @?= Just 1234567900
                b @?= "1234567900"
            it "set initial value notexist" $ withConn $ \c -> do
                when (v < ev) $ pendingWith msg
                a <- McMaybe.increment 0 "notexist" 10 10 c
                b <- McIO.get_ "notexist" c
                a @?= Just 10
                b @?= "10"

        describe "decrement" $ do
            it "return Nothing when decrement non numeric value" $ withConn $ \c -> do
                r <- McMaybe.decrement 0 "foo" 10 10 c
                b <- McIO.get_ "foo" c
                r @?= Nothing
                b @?= "foovalue"
            it "decrement bar" $ withConn $ \c -> do
                a <- McMaybe.decrement 0 "bar" 10 10 c
                b <- McIO.get_ "bar" c
                a @?= Just 1234567880
                b @?= "1234567880"
            it "set initial value notexist" $ withConn $ \c -> do
                when (v < ev) $ pendingWith msg
                a <- McMaybe.decrement 0 "notexist" 10 10 c
                b <- McIO.get_ "notexist" c
                a @?= Just 10
                b @?= "10"
 
  where
    ev  = Version [1,4,17] []
    msg = "memcached(< 1.4.17) bug. see: https://code.google.com/p/memcached/wiki/ReleaseNotes1417"

testFlush :: Spec
testFlush = context "flush" $ do
    context "IO module" $ do
        describe "flushAll" $ do
            it "flush all data" $ withConn $ \c -> do
                McIO.flushAll c
                a <- McMaybe.get "foo" c
                a @?= Nothing

    context "Maybe module" $ do
        describe "flushAll" $ do
            it "flush all data and return True" $ withConn $ \c -> do
                r <- McMaybe.flushAll c
                a <- McMaybe.get "foo" c
                r @?= True
                a @?= Nothing

testVersion :: Spec
testVersion = context "version" $ do
    context "IO module" $ do
        describe "versionString" $
            it "return version bytestring" $ withConn $ \c -> do
                v <- McIO.versionString c
                assertBool (show v ++ " is not version like.") $ isVersionLike v

        describe "version" $
            it "return Version" $ withConn $ \c -> do
                v <- McIO.version c
                assertEqual "version branch length" 3 (length $ versionBranch v)

    context "Maybe module" $ do
        describe "versionString" $
            it "returns version bytestring" $ withConn $ \c -> do
                Just v <- McMaybe.versionString c
                assertBool (show v ++ " is not version like.") $ isVersionLike v
  where
    isVersionLike s0 = isJust $ do
        (_, s1) <- S.readInt s0
        unless (S.head s1 == '.') Nothing
        (_, s2) <- S.readInt (S.tail s1)
        unless (S.head s2 == '.') Nothing
        (_, s3) <- S.readInt (S.tail s2)
        unless (S.null s3) Nothing

testNoOp :: Spec
testNoOp = context "noOp" $ do
    context "IO module" $
        it "is noop" $ withConn McIO.noOp
    context "Maybe module" $
        it "is noop and returns True" $ withConn $ \c -> do
            b <- McMaybe.noOp c
            b @?= True

testAppendPrepend :: Spec
testAppendPrepend = context "append/prepend" $ do
    context "IO module" $ do
        describe "append" $ do
            it "append !! to foo" $ withConn $ \c -> do
                McIO.append "foo" "!!" c
                a <- McIO.get_ "foo" c
                a @?= "foovalue!!"
            it "append !! to bar" $ withConn $ \c -> do
                McIO.append "bar" "!!" c
                a <- McIO.get_ "bar" c
                a @?= "1234567890!!"
            it "throws exception(5) when not exist." $ withConn $ \c ->
                assertException 5 "Not stored." $ McIO.append "notexist" "!!" c
        
        describe "prepend" $ do
            it "prepend !! to foo" $ withConn $ \c -> do
                McIO.prepend "foo" "!!" c
                a <- McIO.get_ "foo" c
                a @?= "!!foovalue"

            it "prepend !! to bar" $ withConn $ \c -> do
                McIO.prepend "bar" "!!" c
                a <- McIO.get_ "bar" c
                a @?= "!!1234567890"

            it "throws exception(5) when not exist" $ withConn $ \c ->
                assertException 5 "Not stored." $ McIO.prepend "notexist" "!!" c

    context "Maybe module" $ do
        describe "append" $ do
            it "append !! to foo and return True" $ withConn $ \c -> do
                b <- McMaybe.append "foo" "!!" c
                a <- McIO.get_ "foo" c
                b @?= True
                a @?= "foovalue!!"

            it "append !! to bar and return True" $ withConn $ \c -> do
                b <- McMaybe.append "bar" "!!" c
                a <- McIO.get_ "bar" c
                b @?= True
                a @?= "1234567890!!"

            it "return False when not exist" $ withConn $ \c -> do
                b <- McMaybe.append "notexist" "!!" c
                b @?= False

        describe "prepend" $ do
            it "prepend !! to foo and return True" $ withConn $ \c -> do
                b <- McMaybe.prepend "foo" "!!" c
                a <- McIO.get_ "foo" c
                b @?= True
                a @?= "!!foovalue"

            it "prepend !! to baar and return True" $ withConn $ \c -> do
                b <- McMaybe.prepend "bar" "!!" c
                a <- McIO.get_ "bar" c
                b @?= True
                a @?= "!!1234567890"

            it "return False when not exist" $ withConn $ \c -> do
                b <- McMaybe.prepend "notexist" "!!" c
                b @?= False

testTouchGAT :: Version -> Spec
testTouchGAT v = context "touch/GAT" $ do
    context "IO module" $ do
        describe "touch" $ do
            it "touch 1s and expire it." $ withConn $ \c -> do
                when (v < ev) $ pendingWith msg
                a <- McMaybe.get_ "foo" c
                McIO.touch 1 "foo" c
                a @?= (Just "foovalue")
                threadDelay 1100000
                b <- McMaybe.get_ "foo" c
                b @?= Nothing

            it "throws exception(1) when not exist" $ withConn $ \c ->
                assertException 1 "Not found" $ McIO.touch 1 "notexist" c

        describe "getAndTouch" $ do
            it "touch 1s, return (flag, value), and expire it" $ withConn $ \c -> do
                when (v < ev) $ pendingWith msg
                x <- McMaybe.get "foo" c
                y <- McIO.getAndTouch 1 "foo" c
                x @?= Just (0, "foovalue")
                y @?= (0, "foovalue")
                threadDelay 1100000
                z <- McMaybe.get "foo" c
                z @?= Nothing

            it "throws exception(1) when not exist" $ withConn $ \c ->
                assertException 1 "Not found" $ McIO.getAndTouch 1 "notexist" c

        describe "getAndTouch_" $ do
            it "touch 1s, return value, and expire it" $ withConn $ \c -> do
                when (v < ev) $ pendingWith msg
                x <- McMaybe.get_ "foo" c
                y <- McIO.getAndTouch_ 1 "foo" c
                x @?= Just "foovalue"
                y @?=      "foovalue"
                threadDelay 1100000
                z <- McMaybe.get_ "foo" c
                z @?= Nothing

            it "throws exception(1) when not exist" $ withConn $ \c ->
                assertException 1 "Not found" $ McIO.getAndTouch_ 1 "notexist" c

    context "Maybe module" $ do
        describe "touch" $ do
            it "touch 1s, return True, and expire it." $ withConn $ \c -> do
                when (v < ev) $ pendingWith msg
                a <- McMaybe.get_ "foo" c
                r <- McMaybe.touch 1 "foo" c
                a @?= (Just "foovalue")
                r @?= True
                threadDelay 1100000
                b <- McMaybe.get_ "foo" c
                b @?= Nothing

            it "return False when not exist" $ withConn $ \c -> do
                r <- McMaybe.touch 1 "notexist" c
                a <- McMaybe.get "notexist" c
                r @?= False
                a @?= Nothing

        describe "getAndTouch" $ do
            it "touch 1s, return (flag, value), and expire it" $ withConn $ \c -> do
                when (v < ev) $ pendingWith msg
                a <- McMaybe.get "foo" c
                r <- McMaybe.getAndTouch 1 "foo" c
                a @?= Just (0, "foovalue")
                r @?= Just (0, "foovalue")
                threadDelay 1100000
                b <- McMaybe.get_ "foo" c
                b @?= Nothing

            it "return Nothing when not exist" $ withConn $ \c -> do
                r <- McMaybe.getAndTouch 1 "notexist" c
                a <- McMaybe.get "notexist" c
                r @?= Nothing
                a @?= Nothing

        describe "getAndTouch_" $ do
            it "touch 1s, return value, and expire it" $ withConn $ \c -> do
                when (v < ev) $ pendingWith msg
                a <- McMaybe.get_ "foo" c
                r <- McMaybe.getAndTouch_ 1 "foo" c
                a @?= Just "foovalue"
                r @?= Just "foovalue"
                threadDelay 1100000
                b <- McMaybe.get_ "foo" c
                b @?= Nothing

            it "return Nothing when not exist" $ withConn $ \c -> do
                r <- McMaybe.getAndTouch_ 1 "notexist" c
                a <- McMaybe.get "notexist" c
                r @?= Nothing
                a @?= Nothing
  where
    ev  = Version [1,4,14] []
    msg = "memcached(< 1.4.14) bug. see: https://code.google.com/p/memcached/wiki/ReleaseNotes1414 & https://code.google.com/p/memcached/issues/detail?id=275"

testModify :: Spec
testModify = context "modify" $ do
    context "IO monad" $ do
        describe "modify" $ do
            it "reverse foo value and return original" $ withConn $ \c -> do
                r <- McIO.modify 0 "foo" (\f v -> (f + 100, L.reverse v, v)) c
                a <- McMaybe.get "foo" c
                r @?= "foovalue"
                a @?= Just (100, "eulavoof")

            it "throw exception(1) when not exist" $ withConn $ \c -> do
                assertException 1 "Not found" $
                    McIO.modify 0 "notexist" (\f v -> (f,v,())) c

        describe "modify_" $ do
            it "reverse foo value" $ withConn $ \c -> do
                McIO.modify_ 0 "foo" (\f v -> (f + 100, L.reverse v)) c
                a <- McMaybe.get "foo" c
                a @?= Just (100, "eulavoof")

            it "throw exception(1) when not exist" $ withConn $ \c -> do
                assertException 1 "Not found" $
                    McIO.modify_ 0 "notexist" (\f v -> (f,v)) c

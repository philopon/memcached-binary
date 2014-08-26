{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception
import Control.Concurrent
import Control.Monad
import Network
import System.Process
import Database.Memcached.Binary
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

main :: IO ()
main = bracket startMemcached terminateProcess $ \_ -> defaultMain
    [ testCase "version" . void $ withConnection def version
    ]

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Database.Memcached.Binary
    ( -- * connection
      I.Connection, I.withConnection, I.connect, I.close
      -- * get
    , get,  get_
    , get', get'_
    , getMaybe, getMaybe_
      -- * set
    , set,  add,  replace
    , set', add', replace'
      -- * delete
    , delete, delete'
      -- * increment/decrement
    , increment,  decrement
    , increment', decrement'
      -- * flush
    , flushAll, flushAll'
    -- * version
    , version, version'
    -- * noOp
    , noOp, noOp'
    -- * append/prepend
    , append,  prepend
    , append', prepend'
    -- * touch
    , touch,  getAndTouch,  getAndTouch_,  getMaybeAndTouch
    , touch', getAndTouch', getAndTouch'_, getMaybeAndTouch_
      -- * modify
    , modify , modify_
      -- * reexports
    , module Database.Memcached.Binary.Types
    -- | def
    , module Data.Default.Class
    -- | PortID(..)
    , module Network
    ) where

import Control.Exception
import Network(PortID(..))

import Data.Default.Class(def)

import qualified Data.ByteString as S

import Database.Memcached.Binary.Types
import Database.Memcached.Binary.Exception
import Database.Memcached.Binary.Internal.Definition
import qualified Database.Memcached.Binary.Internal as I

failureIO :: I.Failure a
failureIO w m = throwIO $ MemcachedException w m

failureMaybe :: I.Failure (Maybe a)
failureMaybe _ _ = return Nothing

failureBool :: I.Failure Bool
failureBool _ _ = return False

--------------------------------------------------------------------------------

-- | get value and flags. if error occured, throw MemcachedException.
get :: Key -> I.Connection -> IO (Flags, Value)
get = I.useConnection . I.get (\_ f v -> return (f,v)) failureIO

-- | get value and flags. if error occured, return Nothing.
--
-- @
-- get' == getMaybe
-- @
getMaybe, get' :: Key -> I.Connection -> IO (Maybe (Flags, Value))
getMaybe = I.useConnection . I.get (\_ f v -> return $ Just (f,v)) failureMaybe
get' = getMaybe

-- | get value. if error occured, throw MemcachedException.
get_ :: Key -> I.Connection -> IO Value
get_ = I.useConnection . I.get (\_ _ v -> return v) failureIO

-- | get value. if error occured, return Nothing.
--
-- @
-- get'_ == getMaybe_
-- @
getMaybe_, get'_ :: Key -> I.Connection -> IO (Maybe Value)
getMaybe_ = I.useConnection . I.get (\_ _ v -> return $ Just v) failureMaybe
get'_ = getMaybe_

--------------------------------------------------------------------------------

setAddReplace :: OpCode -> Flags -> Expiry
              -> Key -> Value -> I.Connection -> IO ()
setAddReplace op = \f e key value -> I.useConnection $
    I.setAddReplace (const $ return ()) failureIO op (CAS 0) key value f e

-- | set value. if error occured, throw MemcachedException.
set :: Flags -> Expiry -> Key -> Value -> I.Connection -> IO ()
set = setAddReplace opSet

-- | add value. if error occured, throw MemcachedException.
add :: Flags -> Expiry -> Key -> Value -> I.Connection -> IO ()
add = setAddReplace opAdd

-- | replace value. if error occured, throw MemcachedException.
replace :: Flags -> Expiry -> Key -> Value -> I.Connection -> IO ()
replace = setAddReplace opReplace

setAddReplace' :: OpCode -> Flags -> Expiry
               -> Key -> Value -> I.Connection -> IO Bool
setAddReplace' op = \f e key value -> I.useConnection $
    I.setAddReplace (const $ return True) failureBool op (CAS 0) key value f e


-- | set value. if error occured, return False.
set' :: Flags -> Expiry -> Key -> Value -> I.Connection -> IO Bool
set' = setAddReplace' opSet

-- | add value. if error occured, return False.
add' :: Flags -> Expiry -> Key -> Value -> I.Connection -> IO Bool
add' = setAddReplace' opAdd

-- | replace value. if error occured, return False.
replace' :: Flags -> Expiry -> Key -> Value -> I.Connection -> IO Bool
replace' = setAddReplace' opReplace

--------------------------------------------------------------------------------

-- | delete value. if error occured, throw MemcachedException.
delete :: Key -> I.Connection -> IO ()
delete = I.useConnection . I.delete (\_ -> return ()) failureIO (CAS 0)

-- | delete value. if error occured, return False.
delete' :: Key -> I.Connection -> IO Bool
delete' = I.useConnection . I.delete (\_ -> return True) failureBool (CAS 0)

--------------------------------------------------------------------------------

-- | modify value in transaction. if error occured, throw MemcachedException.
modify :: Expiry -> Key -> (Flags -> Value -> (Flags, Value, a))
       -> I.Connection -> IO a
modify e key fn = I.useConnection $ \h ->
    I.get (\c f v -> 
        let (f', v', r) = fn f v
        in I.setAddReplace (const $ return r) failureIO opSet c key v' f' e h
    ) failureIO key h

-- | modify value in transaction. if error occured, throw MemcachedException.
modify_ :: Expiry
        -> Key -> (Flags -> Value -> (Flags, Value))
        -> I.Connection -> IO ()
modify_ e key fn = I.useConnection $ \h ->
    I.get (\c f v -> 
        let (f', v') = fn f v
        in I.setAddReplace (const $ return ()) failureIO opSet c key v' f' e h
    ) failureIO key h

--------------------------------------------------------------------------------

incrDecr :: OpCode -> Expiry
         -> Key -> Delta -> Initial -> I.Connection -> IO Counter
incrDecr op = \e k d i -> I.useConnection $
    I.incrDecr (\_ w -> return w) failureIO op (CAS 0) k d i e

incrDecr' :: OpCode -> Expiry
          -> Key -> Delta -> Initial -> I.Connection -> IO (Maybe Counter)
incrDecr' op e k d i = I.useConnection $
    I.incrDecr (\_ w -> return $ Just w) failureMaybe op (CAS 0) k d i e


-- | increment value. if error occured, throw MemcachedException.
increment :: Expiry -> Key -> Delta -> Initial -> I.Connection -> IO Counter
increment = incrDecr opIncrement

-- | decrement value. if error occured, throw MemcachedException.
decrement :: Expiry -> Key -> Delta -> Initial -> I.Connection -> IO Counter
decrement = incrDecr opDecrement

-- | increment value. if error occured, return Nothing.
increment' :: Expiry -> Key -> Delta -> Initial
           -> I.Connection -> IO (Maybe Counter)
increment' = incrDecr' opIncrement

-- | decrement value. if error occured, return Nothing.
decrement' :: Expiry -> Key -> Delta -> Initial
           -> I.Connection -> IO (Maybe Counter)
decrement' = incrDecr' opDecrement

--------------------------------------------------------------------------------

-- | flush all value. if error occured, throw MemcachedException.
flushAll :: I.Connection -> IO ()
flushAll = I.useConnection $ I.flushAll (return ()) failureIO

-- | flush all value. if error occured, return False.
flushAll' :: I.Connection -> IO Bool
flushAll' = I.useConnection $ I.flushAll (return True) failureBool

--------------------------------------------------------------------------------

-- | get version string. if error occured, throw MemcachedException.
version :: I.Connection -> IO S.ByteString
version = I.useConnection $ I.version return failureIO

-- | get version string. if error occured, return False.
version' :: I.Connection -> IO (Maybe S.ByteString)
version' = I.useConnection $ I.version (return . Just) failureMaybe

--------------------------------------------------------------------------------

-- | noop(use for keepalive). if error occured, throw MemcachedException.
noOp :: I.Connection -> IO ()
noOp  = I.useConnection $ I.noOp (return ())   failureIO

-- | noop(use for keepalive). if error occured, return False.
noOp' :: I.Connection -> IO Bool
noOp' = I.useConnection $ I.noOp (return True) failureBool

--------------------------------------------------------------------------------

appendPrepend :: OpCode -> Key -> Value -> I.Connection -> IO ()
appendPrepend o = \k v -> I.useConnection $
    I.appendPrepend (\_ -> return ()) failureIO o (CAS 0) k v

appendPrepend' :: OpCode -> Key -> Value -> I.Connection -> IO Bool
appendPrepend' o = \k v -> I.useConnection $
    I.appendPrepend (\_ -> return False) failureBool o (CAS 0) k v

-- | apeend value. if error occured, throw MemcachedException.
append :: Key -> Value -> I.Connection -> IO ()
append  = appendPrepend opAppend

-- | prepend value. if error occured, throw MemcachedException.
prepend :: Key -> Value -> I.Connection -> IO ()
prepend = appendPrepend opPrepend

-- | append value. if error occured, return False.
append' :: Key -> Value -> I.Connection -> IO Bool
append'  = appendPrepend' opAppend

-- | preppend value. if error occured, return False.
prepend' :: Key -> Value -> I.Connection -> IO Bool
prepend' = appendPrepend' opPrepend

--------------------------------------------------------------------------------

-- | change expiry. if error occured, throw MemcachedException.
touch :: Key -> Expiry -> I.Connection -> IO ()
touch k e = I.useConnection $
    I.touch (\_ _ _ -> return ()) failureIO opTouch k e

-- | change expiry. if error occured, return False.
touch' :: Key -> Expiry -> I.Connection -> IO Bool
touch' k e = I.useConnection $
    I.touch (\_ _ _ -> return True) failureBool opTouch k e

-- | get value and flags, then change expiry. 
-- if error occured, throw MemcachedException.
getAndTouch :: Key -> Expiry -> I.Connection -> IO (Flags, Value)
getAndTouch k e = I.useConnection $
    I.touch (\_ f v -> return (f,v)) failureIO opGAT k e

-- | get value and flags, then change expiry. 
-- if error occured, return Nothing.
--
-- @
-- getMaybeAndTouch == getAndTouch'
-- @
getAndTouch', getMaybeAndTouch
    :: Key -> Expiry -> I.Connection -> IO (Maybe (Flags, Value))
getAndTouch' k e = I.useConnection $
    I.touch (\_ f v -> return $ Just (f,v)) failureMaybe opGAT k e
getMaybeAndTouch = getAndTouch'

-- | get value then change expiry. 
-- if error occured, throw MemcachedException.
getAndTouch_ :: Key -> Expiry -> I.Connection -> IO Value
getAndTouch_ k e = I.useConnection $
    I.touch (\_ _ v -> return v) failureIO opGAT k e

-- | get value then change expiry. 
-- if error occured, return Nothing.
--
-- @
-- getMaybeAndTouch_ == getAndTouch'_
-- @
getAndTouch'_, getMaybeAndTouch_
    :: Key -> Expiry -> I.Connection -> IO (Maybe Value)
getAndTouch'_ k e = I.useConnection $
    I.touch (\_ _ v -> return $ Just v) failureMaybe opGAT k e
getMaybeAndTouch_ = getAndTouch'_

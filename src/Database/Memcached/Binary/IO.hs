{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}

module Database.Memcached.Binary.IO
#include "Header.txt"

import Control.Exception

#define NoReturn  ()
#define HasReturn

successHasReturn :: a -> IO (HasReturn a)
successHasReturn = return
{-# INLINE successHasReturn #-}

successNoReturn :: IO NoReturn
successNoReturn  = return ()
{-# INLINE successNoReturn #-}

failureHasReturn :: I.Failure (HasReturn a)
failureHasReturn i m = throwIO $ MemcachedException i m
{-# INLINE failureHasReturn #-}

failureNoReturn :: I.Failure NoReturn
failureNoReturn i m = throwIO $ MemcachedException i m
{-# INLINE failureNoReturn #-}

#include "Common.hs"


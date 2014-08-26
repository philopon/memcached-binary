{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}

module Database.Memcached.Binary.Either
#include "Header.txt"

#define NoReturn  (Maybe MemcachedException)
#define HasReturn Either MemcachedException

successHasReturn :: a -> IO (HasReturn a)
successHasReturn = return . Right
{-# INLINE successHasReturn #-}

successNoReturn :: IO NoReturn
successNoReturn  = return Nothing
{-# INLINE successNoReturn #-}

failureHasReturn :: I.Failure (HasReturn a)
failureHasReturn i m = return . Left $ MemcachedException i m
{-# INLINE failureHasReturn #-}

failureNoReturn :: I.Failure NoReturn
failureNoReturn i m = return . Just $ MemcachedException i m
{-# INLINE failureNoReturn #-}

#include "Common.hs"


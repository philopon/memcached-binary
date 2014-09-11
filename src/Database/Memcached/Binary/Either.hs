{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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
failureHasReturn = return . Left
{-# INLINE failureHasReturn #-}

failureNoReturn :: I.Failure NoReturn
failureNoReturn = return . Just
{-# INLINE failureNoReturn #-}
#include "Common.hs"


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}

module Database.Memcached.Binary.Maybe
#include "Header.txt"

#define NoReturn  Bool
#define HasReturn Maybe

successHasReturn :: a -> IO (HasReturn a)
successHasReturn = return . Just
{-# INLINE successHasReturn #-}

successNoReturn :: IO NoReturn
successNoReturn  = return True
{-# INLINE successNoReturn #-}

failureHasReturn :: I.Failure (HasReturn a)
failureHasReturn _ = return Nothing
{-# INLINE failureHasReturn #-}

failureNoReturn :: I.Failure NoReturn
failureNoReturn _ = return False
{-# INLINE failureNoReturn #-}

#include "Common.hs"

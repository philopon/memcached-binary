{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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
failureHasReturn = throwIO
{-# INLINE failureHasReturn #-}

failureNoReturn :: I.Failure NoReturn
failureNoReturn = throwIO
{-# INLINE failureNoReturn #-}

#include "Common.hs"


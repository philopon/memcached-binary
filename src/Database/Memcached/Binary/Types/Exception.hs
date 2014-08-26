{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Database.Memcached.Binary.Types.Exception where

import Control.Exception
import Data.Word
import Data.Typeable
import qualified Data.ByteString as S

data MemcachedException = MemcachedException
    {-# UNPACK #-} !Word16 {-# UNPACK #-} !S.ByteString
    deriving (Show, Typeable)

instance Exception MemcachedException

#define defExceptionP(n,w) n :: MemcachedException -> Bool;\
n (MemcachedException i _) = i == w

defExceptionP(isKeyNotFound                   , 0x01)
defExceptionP(isKeyExists                     , 0x02)
defExceptionP(isValueTooLarge                 , 0x03)
defExceptionP(isInvalidArguments              , 0x04)
defExceptionP(isItemNotStored                 , 0x05)
defExceptionP(isIncrDecrOnNonNumeric          , 0x06)
defExceptionP(isVBucketBelongsToAnotherServer , 0x07)
defExceptionP(isAuthenticationError           , 0x08)
defExceptionP(isAuthenticationContinue        , 0x09)
defExceptionP(isUnknownCommand                , 0x81)
defExceptionP(isOutOfMemory                   , 0x82)
defExceptionP(isNotSupported                  , 0x83)
defExceptionP(isInternalError                 , 0x84)
defExceptionP(isBusy                          , 0x85)
defExceptionP(isTemporaryFailure              , 0x86)

#undef defExceptionP

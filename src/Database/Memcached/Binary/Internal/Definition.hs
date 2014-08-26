{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module Database.Memcached.Binary.Internal.Definition where

import Foreign.Storable
import Data.Word

newtype OpCode = OpCode Word8 deriving (Storable)

#define defOpCode(n,w) n :: OpCode; n = OpCode w

defOpCode(opGet               , 0x00)
defOpCode(opSet               , 0x01)
defOpCode(opAdd               , 0x02)
defOpCode(opReplace           , 0x03)
defOpCode(opDelete            , 0x04)
defOpCode(opIncrement         , 0x05)
defOpCode(opDecrement         , 0x06)
defOpCode(opQuit              , 0x07)
defOpCode(opFlush             , 0x08)
defOpCode(opGetQ              , 0x09)
defOpCode(opNoOp              , 0x0a)
defOpCode(opVersion           , 0x0b)
defOpCode(opGetK              , 0x0c)
defOpCode(opGetKQ             , 0x0d)
defOpCode(opAppend            , 0x0e)
defOpCode(opPrepend           , 0x0f)

defOpCode(opStat              , 0x10)
defOpCode(opSetQ              , 0x11)
defOpCode(opAddQ              , 0x12)
defOpCode(opReplaceQ          , 0x13)
defOpCode(opDeleteQ           , 0x14)
defOpCode(opIncrementQ        , 0x15)
defOpCode(opDecrementQ        , 0x16)
defOpCode(opQuitQ             , 0x17)
defOpCode(opFlushQ            , 0x18)
defOpCode(opAppendQ           , 0x19)
defOpCode(opPrependQ          , 0x1a)
defOpCode(opVerbosity         , 0x1b)
defOpCode(opTouch             , 0x1c)
defOpCode(opGAT               , 0x1d)
defOpCode(opGATQ              , 0x1e)

defOpCode(opSaslListMechs     , 0x20)
defOpCode(opSaslAuth          , 0x21)
defOpCode(opSaslStep          , 0x22)

defOpCode(opRGet              , 0x30)
defOpCode(opRSet              , 0x31)
defOpCode(opRSetQ             , 0x32)
defOpCode(opRAppend           , 0x33)
defOpCode(opRAppendQ          , 0x34)
defOpCode(opRPrepend          , 0x35)
defOpCode(opRPrependQ         , 0x36)
defOpCode(opRDelete           , 0x37)
defOpCode(opRDeleteQ          , 0x38)
defOpCode(opRIncr             , 0x39)
defOpCode(opRIncrQ            , 0x3a)
defOpCode(opRDecr             , 0x3b)
defOpCode(opRDecrQ            , 0x3c)
defOpCode(opSetVBucket        , 0x3d)
defOpCode(opGetVBucket        , 0x3e)
defOpCode(opDelVBucket        , 0x3f)

defOpCode(opTAPConnect        , 0x40)
defOpCode(opTAPMutation       , 0x41)
defOpCode(opTAPDelete         , 0x42)
defOpCode(opTAPFlush          , 0x43)
defOpCode(opTAPOpaque         , 0x44)
defOpCode(opTAPVBucketSet     , 0x45)
defOpCode(opTAPCheckpointStart, 0x46)
defOpCode(opTAPCheckpointEnd  , 0x47)

#undef defOpCode

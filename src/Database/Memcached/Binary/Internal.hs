{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Database.Memcached.Binary.Internal where

import Network

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc

import System.IO

import Control.Monad
import Control.Exception

import Data.Word
import Data.Pool
import Data.Storable.Endian
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as S

import Database.Memcached.Binary.Types
import Database.Memcached.Binary.Types.Exception
import Database.Memcached.Binary.Internal.Definition

newtype Connection = Connection (Pool Handle)

withConnection :: ConnectInfo -> (Connection -> IO a) -> IO a
withConnection i m = withSocketsDo $ bracket (connect i) close m

connect :: ConnectInfo -> IO Connection
connect i = fmap Connection $ 
    createPool (putStrLn "open" >> connect' i) (\h -> putStrLn "closed" >> quit h >> hClose h) 1 
    (connectionIdleTime i) (numConnection i)

connect' :: ConnectInfo -> IO Handle
connect' i = loop (connectAuth i)
  where
    loop [] = do
        connectTo (connectHost i) (connectPort i)

    loop [a] = do
        h <- connectTo (connectHost i) (connectPort i)
        auth a (\_ -> return h) (\w m -> throwIO $ MemcachedException w m) h

    loop (a:as) = do
        h <- connectTo (connectHost i) (connectPort i)
        handle (\(_::IOError) -> loop as) $
            auth a (\_ -> return h) (\_ _ -> loop as) h

close :: Connection -> IO ()
close (Connection p) = destroyAllResources p

useConnection :: (Handle -> IO a) -> Connection -> IO a
useConnection f (Connection p) = withResource p f

pokeWord8 :: Ptr a -> Word8 -> IO ()
pokeWord8 = poke . castPtr

pokeWord16be :: Ptr a -> Word16 -> IO ()
pokeWord16be p w = poke (castPtr p) (BE w)

pokeWord32be :: Ptr a -> Word32 -> IO ()
pokeWord32be p w = poke (castPtr p) (BE w)

pokeWord64be :: Ptr a -> Word64 -> IO ()
pokeWord64be p w = poke (castPtr p) (BE w)

peekWord8 :: Ptr a -> IO Word8
peekWord8 = peek . castPtr

peekWord16be :: Ptr a -> IO Word16
peekWord16be p = peek (castPtr p) >>= \(BE w) -> return w

peekWord32be :: Ptr a -> IO Word32
peekWord32be p = peek (castPtr p) >>= \(BE w) -> return w

peekWord64be :: Ptr a -> IO Word64
peekWord64be p = peek (castPtr p) >>= \(BE w) -> return w

pokeByteString :: Ptr a -> S.ByteString -> IO ()
pokeByteString p v =
    S.unsafeUseAsCString v $ \cstr ->
        copyBytes (castPtr p) cstr (S.length v)

pokeLazyByteString :: Ptr a -> L.ByteString -> IO ()
pokeLazyByteString p v =
    void $ L.foldlChunks (\mi s -> mi >>= \i -> do
        pokeByteString (plusPtr p i) s
        return $ i + S.length s
    ) (return 0) v

data Header
data Request

mallocRequest :: OpCode -> Key -> Word8 -> (Ptr Request -> IO ())
              -> Int -> (Ptr Request -> IO ()) -> Word32 -> CAS -> IO (Ptr Request)
mallocRequest (OpCode o) key elen epoke vlen vpoke opaque (CAS cas) = do
    let tlen = S.length key + fromIntegral elen + vlen
    p <- mallocBytes (24 + fromIntegral tlen)
    pokeWord8             p     0x80
    pokeWord8    (plusPtr p  1) o
    pokeWord16be (plusPtr p  2) (fromIntegral $ S.length key)
    pokeWord8    (plusPtr p  4) elen
    pokeWord8    (plusPtr p  5) 0x00
    pokeWord16be (plusPtr p  6) 0x00
    pokeWord32be (plusPtr p  8) (fromIntegral tlen)
    pokeWord32be (plusPtr p 12) opaque
    pokeWord64be (plusPtr p 16) cas
    epoke (plusPtr p 24)
    pokeByteString (plusPtr p $ 24 + fromIntegral elen) key
    vpoke (plusPtr p $ 24 + fromIntegral elen + S.length key)
    return p
{-# INLINE mallocRequest #-}

sendRequest :: OpCode -> Key -> Word8 -> (Ptr Request -> IO ())
            -> Int -> (Ptr Request -> IO ()) -> Word32 -> CAS -> Handle -> IO ()
sendRequest op key elen epoke vlen vpoke opaque cas h =
    bracket (mallocRequest op key elen epoke vlen vpoke opaque cas) free $ \req -> do
        hPutBuf h req (24 + S.length key + fromIntegral elen + vlen)
        hFlush h
{-# INLINE sendRequest #-}

type Failure a = Word16 -> S.ByteString -> IO a

peekResponse :: (Ptr Header -> IO a) -> Failure a -> Handle -> IO a
peekResponse success failure h = allocaBytes 24 $ \p ->
    hGetBuf h p 24 >> peekWord16be (plusPtr p 6) >>= \st ->
    if st == 0
        then success p
        else do
            bl <- peekWord32be (plusPtr p 8)
            failure st =<< S.hGet h (fromIntegral bl)
{-# INLINE peekResponse #-}

withRequest :: OpCode -> Key -> Word8 -> (Ptr Request -> IO ())
            -> Int -> (Ptr Request -> IO ()) -> CAS
            -> (Handle -> Ptr Header -> IO a) -> Failure a -> Handle -> IO a
withRequest op key elen epoke vlen vpoke cas success failure h = do
    sendRequest  op key elen epoke vlen vpoke 0 cas h
    peekResponse (success h) failure h

getExtraLength :: Ptr Header -> IO Word8
getExtraLength p = peekWord8 (plusPtr p 4)

getKeyLength :: Ptr Header -> IO Word16
getKeyLength p = peekWord16be (plusPtr p 2)

getTotalLength :: Ptr Header -> IO Word32
getTotalLength p = peekWord32be (plusPtr p 8)

getCAS :: Ptr Header -> IO CAS
getCAS p = fmap CAS $ peekWord64be (plusPtr p 16)

getOpaque :: Ptr Header -> IO Word32
getOpaque p = peekWord32be (plusPtr p 12)

nop :: Ptr Request -> IO ()
nop _ = return ()

inspectResponse :: Handle -> Ptr Header 
                -> IO (S.ByteString, S.ByteString, L.ByteString)
inspectResponse h p = do
    el <- getExtraLength p
    kl <- getKeyLength   p
    tl <- getTotalLength p
    e <- S.hGet h $ fromIntegral el
    k <- S.hGet h $ fromIntegral kl
    v <- L.hGet h $ fromIntegral tl - fromIntegral el - fromIntegral kl
    return (e,k,v)

getSuccessCallback :: (Flags -> Value -> IO a)
                   -> Handle -> Ptr Header -> IO a
getSuccessCallback success h p = do
    elen <- getExtraLength p
    tlen <- getTotalLength p
    void $ hGetBuf h p 4
    flags <- peekWord32be p
    value <- L.hGet h (fromIntegral tlen - fromIntegral elen)
    success flags value

get :: (Flags -> Value -> IO a) -> Failure a
    -> Key -> Handle -> IO a
get success failure key = 
    withRequest opGet key 0 nop 0 nop (CAS 0)
    (getSuccessCallback success) failure

getWithCAS :: (CAS -> Flags -> Value -> IO a) -> Failure a
           -> Key -> Handle -> IO a
getWithCAS success failure key =
    withRequest opGet key 0 nop 0 nop (CAS 0)
    (\h p -> getCAS p >>= \c -> getSuccessCallback (success c) h p) failure

setAddReplace :: IO a -> Failure a -> OpCode -> CAS
              -> Key -> Value -> Flags -> Expiry -> Handle -> IO a
setAddReplace success failure o cas key value flags expiry = withRequest o key
        8 (\p -> pokeWord32be p flags >> pokeWord32be (plusPtr p 4) expiry) 
        (fromIntegral $ L.length value) (flip pokeLazyByteString value) cas (\_ _ -> success) failure

setAddReplaceWithCAS :: (CAS -> IO a) -> Failure a -> OpCode -> CAS
                     -> Key -> Value -> Flags -> Expiry -> Handle -> IO a
setAddReplaceWithCAS success failure o cas key value flags expiry = withRequest o key
        8 (\p -> pokeWord32be p flags >> pokeWord32be (plusPtr p 4) expiry) 
        (fromIntegral $ L.length value) (flip pokeLazyByteString value) cas (\_ p -> getCAS p >>= success) failure

delete :: IO a -> Failure a -> CAS -> Key -> Handle -> IO a
delete success failure cas key =
    withRequest opDelete key 0 nop 0 nop cas (\_ _ -> success) failure

incrDecr :: (Word64 -> IO a) -> Failure a -> OpCode -> CAS
         -> Key -> Delta -> Initial -> Expiry -> Handle -> IO a
incrDecr success failure op cas key delta initial expiry =
    withRequest op key 20 extra 0 nop cas success' failure
  where
    extra p = do
        pokeWord64be          p     delta
        pokeWord64be (plusPtr p  8) initial
        pokeWord32be (plusPtr p 16) expiry

    success' h p = do
        void $ hGetBuf h p 8
        peekWord64be p >>= success

quit :: Handle -> IO ()
quit h = do
    sendRequest  opQuit "" 0 nop 0 nop 0 (CAS 0) h
    peekResponse (\_ -> return ()) (\_ _ -> return ()) h

flushAll :: IO a -> Failure a -> Handle -> IO a
flushAll success =
    withRequest opFlush "" 0 nop 0 nop (CAS 0) (\_ _ -> success)

flushWithin :: IO a -> Failure a -> Expiry -> Handle -> IO a
flushWithin success failure w =
    withRequest opFlush "" 4 (flip pokeWord32be w) 0 nop (CAS 0)
    (\_ _ -> success) failure

noOp :: IO a -> Failure a -> Handle -> IO a
noOp success =
    withRequest opNoOp "" 0 nop 0 nop (CAS 0) (\_ _ -> success)

version :: (S.ByteString -> IO a) -> Failure a -> Handle -> IO a
version success =
    withRequest opVersion "" 0 nop 0 nop (CAS 0)
    (\h p -> getTotalLength p >>= S.hGet h . fromIntegral >>= success)

appendPrepend :: IO a -> Failure a -> OpCode -> CAS
              -> Key -> Value -> Handle -> IO a
appendPrepend success failure op cas key value = withRequest op key 0 nop
    (fromIntegral $ L.length value) (flip pokeLazyByteString value)
    cas (\_ _ -> success) failure

stats :: Handle -> IO (H.HashMap S.ByteString S.ByteString)
stats h = loop H.empty
  where
    loop m = do
        sendRequest opStat "" 0 nop 0 nop 0 (CAS 0) h
        peekResponse (success m) (\w s -> throwIO $ MemcachedException w s) h

    success m p = getTotalLength p >>= \tl ->
        if tl == 0
            then return m
            else do
                kl <- getKeyLength p
                k  <- S.hGet h (fromIntegral kl)
                v  <- S.hGet h (fromIntegral tl - fromIntegral kl)
                loop (H.insert k v m)

verbosity :: IO a -> Failure a -> Word32 -> Handle -> IO a
verbosity success failure v = withRequest opVerbosity ""
    4 (flip pokeWord32be v) 0 nop (CAS 0) (\_ _ -> success) failure

touch :: (Flags -> Value -> IO a) -> Failure a -> OpCode
      -> Key -> Expiry -> Handle -> IO a
touch success failure op key e =
    withRequest op key 4 (flip pokeWord32be e) 0 nop (CAS 0)
    (getSuccessCallback success) failure

saslListMechs :: (S.ByteString -> IO a) -> Failure a
              -> Handle -> IO a
saslListMechs success failure =
    withRequest opSaslListMechs "" 0 nop 0 nop (CAS 0)
    (\h p -> getTotalLength p >>= S.hGet h . fromIntegral >>= success)
    failure

auth :: Auth -> (S.ByteString -> IO a) -> Failure a -> Handle -> IO a
auth (Plain u w) success next h = do
    sendRequest  opSaslAuth "PLAIN" 0 nop (S.length u + S.length w + 2) pokeCred 0 (CAS 0) h
    peekResponse consumeResponse next h
  where
    ul = S.length u
    pokeCred p = do
        pokeWord8 p 0
        pokeByteString (plusPtr p        1) u
        pokeWord8      (plusPtr p $ ul + 1) 0
        pokeByteString (plusPtr p $ ul + 2) w

    consumeResponse p = do
        l <- getTotalLength p
        success =<< S.hGet h (fromIntegral l)

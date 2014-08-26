module Database.Memcached.Binary.Types where

import Network

import Data.Time.Clock
import Data.Word
import Data.Default.Class
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

type User     = S.ByteString
type Password = S.ByteString

data Auth
    = Plain User Password
    deriving Show

data ConnectInfo = ConnectInfo
    { connectHost        :: HostName
    , connectPort        :: PortID
    , connectAuth        :: [Auth]
    , numConnection      :: Int
    , connectionIdleTime :: NominalDiffTime
    } deriving Show

instance Default ConnectInfo where
    def = ConnectInfo "localhost" (PortNumber 11211) [] 1 20

type Flags  = Word32
type Key    = S.ByteString
type Value  = L.ByteString
type Expiry = Word32

newtype CAS = CAS Word64 deriving (Show)

type Delta   = Word64
type Initial = Word64
type Counter = Word64

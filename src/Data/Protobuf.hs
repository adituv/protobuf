module Data.Protobuf
  (
  -- * Reexported types
    ByteString
  , Text
  , Int32
  , Int64
  , Word32
  , Word64
  ) where

import Data.Protobuf.Types

import           Data.ByteString (ByteString)
import           Data.Int        (Int32, Int64)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import           Data.Word       (Word32, Word64)

class ProtoMessage m where
  fromProto :: RawMessage -> Result m
  toProto :: m -> RawMessage

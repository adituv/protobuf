module Data.Protobuf
  (
    ProtoMessage(..)
  -- * Reexported types
  , ByteString
  , Text
  , Int32
  , Int64
  , Word32
  , Word64
  -- * Reexported functions used in message definitions
  , (&)
  ) where

import Data.Protobuf.Types

import           Data.ByteString (ByteString)
import           Data.Function   ((&))
import           Data.Int        (Int32, Int64)
import           Data.Text       (Text)
import           Data.Word       (Word32, Word64)

class ProtoMessage m where
  fromProto :: RawMessage -> Result m
  toProto :: m -> RawMessage

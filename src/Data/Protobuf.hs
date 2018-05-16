module Data.Protobuf
  (
    ProtoMessage(..)
  , encode
  , decode
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

import Data.Protobuf.Encoding
import Data.Protobuf.Types

import Data.ByteString (ByteString)
import Data.Serialize  (runGet, runPut)
import Data.Function   ((&))
import Data.Int        (Int32, Int64)
import Data.Text       (Text)
import Data.Word       (Word32, Word64)

class ProtoMessage m where
  fromProto :: RawMessage -> Result m
  toProto :: m -> RawMessage

encode :: ProtoMessage m => m -> ByteString
encode message = runPut $ putRawMessage (toProto message)

decode :: ProtoMessage m => ByteString -> Either String m
decode bytes = case fmap fromProto raw of
    (Left str) -> Left str
    Right (Failure str) -> Left str
    Right (Success a) -> Right a
  where
    raw = runGet getRawMessage bytes

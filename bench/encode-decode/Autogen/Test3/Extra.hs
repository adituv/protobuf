{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Autogen.Test3.Extra(module Data.Protobuf, module Autogen.Test3.Extra) where

import Data.Protobuf
import Data.Protobuf.Encoding
import qualified Data.IntMap as IntMap
import GHC.Generics(Generic)

data Extra = Extra
  { blah :: !Text
  } deriving (Show, Generic)

instance ProtoMessage Autogen.Test3.Extra.Extra where
  fromProto raw =
    Autogen.Test3.Extra.Extra
      <$> raw .: 1
  toProto Autogen.Test3.Extra.Extra{..} = RawMessage
    $ IntMap.empty
      & IntMap.insert 1 [toRawValue blah]

instance AsRawValue Autogen.Test3.Extra.Extra where
  defaultValue =
    Autogen.Test3.Extra.Extra
      defaultValue
  rawType = RTLengthEncoded
  toRawValue msg = RLengthEncoded . runPut $ putRawMessage (toProto msg)
  fromRawValue (RLengthEncoded raw) = case runGet getRawMessage raw of
    Right rawMessage -> fromProto rawMessage
    Left err         -> Failure err
  fromRawValue _ = rawTypeFailure


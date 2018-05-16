{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Autogen.Test3(module Data.Protobuf, module Autogen.Test3) where

import Data.Protobuf
import Data.Protobuf.Encoding
import qualified Data.IntMap as IntMap
import GHC.Generics(Generic)
import Autogen.Test3.Result
import Autogen.Test3.Extra

data Test3 = Test3
  { test1 :: !Word32
  , completed :: !Bool
  } deriving (Show, Generic)

instance ProtoMessage Autogen.Test3.Test3 where
  fromProto raw =
    Autogen.Test3.Test3
      <$> raw .: 5
      <*> raw .: 21
  toProto Autogen.Test3.Test3{..} = RawMessage
    $ IntMap.empty
      & IntMap.insert 5 [toRawValue test1]
      & IntMap.insert 21 [toRawValue completed]

instance AsRawValue Autogen.Test3.Test3 where
  defaultValue =
    Autogen.Test3.Test3
      defaultValue
      defaultValue
  rawType = RTLengthEncoded
  toRawValue msg = RLengthEncoded . runPut $ putRawMessage (toProto msg)
  fromRawValue (RLengthEncoded raw) = case runGet getRawMessage raw of
    Right rawMessage -> fromProto rawMessage
    Left err         -> Failure err
  fromRawValue _ = rawTypeFailure


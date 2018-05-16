{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Autogen.Test3.Result(module Data.Protobuf, module Autogen.Test3.Result) where

import Data.Protobuf
import Data.Protobuf.Encoding
import qualified Data.IntMap as IntMap
import GHC.Generics(Generic)

data Result = Result
  { _data :: !Word32
  } deriving (Show, Generic)

instance ProtoMessage Autogen.Test3.Result.Result where
  fromProto raw =
    Autogen.Test3.Result.Result
      <$> raw .: 1
  toProto Autogen.Test3.Result.Result{..} = RawMessage
    $ IntMap.empty
      & IntMap.insert 1 [toRawValue _data]

instance AsRawValue Autogen.Test3.Result.Result where
  defaultValue =
    Autogen.Test3.Result.Result
      defaultValue
  rawType = RTLengthEncoded
  toRawValue msg = RLengthEncoded . runPut $ putRawMessage (toProto msg)
  fromRawValue (RLengthEncoded raw) = case runGet getRawMessage raw of
    Right rawMessage -> fromProto rawMessage
    Left err         -> Failure err
  fromRawValue _ = rawTypeFailure


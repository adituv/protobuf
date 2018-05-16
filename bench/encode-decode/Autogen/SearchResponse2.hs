{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Autogen.SearchResponse2(module Data.Protobuf, module Autogen.SearchResponse2) where

import Data.Protobuf
import Data.Protobuf.Encoding
import qualified Data.IntMap as IntMap
import GHC.Generics(Generic)
import Autogen.SearchResponse2.Result

data SearchResponse2 = SearchResponse2
  { results :: ![Autogen.SearchResponse2.Result.Result]
  } deriving (Show, Generic)

instance ProtoMessage Autogen.SearchResponse2.SearchResponse2 where
  fromProto raw =
    Autogen.SearchResponse2.SearchResponse2
      <$> raw .: 1
  toProto Autogen.SearchResponse2.SearchResponse2{..} = RawMessage
    $ IntMap.empty
      & IntMap.insert 1 [toRawValue results]

instance AsRawValue Autogen.SearchResponse2.SearchResponse2 where
  defaultValue =
    Autogen.SearchResponse2.SearchResponse2
      defaultValue
  rawType = RTLengthEncoded
  toRawValue msg = RLengthEncoded . runPut $ putRawMessage (toProto msg)
  fromRawValue (RLengthEncoded raw) = case runGet getRawMessage raw of
    Right rawMessage -> fromProto rawMessage
    Left err         -> Failure err
  fromRawValue _ = rawTypeFailure


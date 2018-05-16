{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Autogen.SearchResponse2.Result(module Data.Protobuf, module Autogen.SearchResponse2.Result) where

import Data.Protobuf
import Data.Protobuf.Encoding
import qualified Data.IntMap as IntMap
import GHC.Generics(Generic)

data Result = Result
  { url :: !Text
  , title :: !Text
  , snippets :: ![Text]
  } deriving (Show, Generic)

instance ProtoMessage Autogen.SearchResponse2.Result.Result where
  fromProto raw =
    Autogen.SearchResponse2.Result.Result
      <$> raw .: 1
      <*> raw .: 2
      <*> raw .: 3
  toProto Autogen.SearchResponse2.Result.Result{..} = RawMessage
    $ IntMap.empty
      & IntMap.insert 1 [toRawValue url]
      & IntMap.insert 2 [toRawValue title]
      & IntMap.insert 3 [toRawValue snippets]

instance AsRawValue Autogen.SearchResponse2.Result.Result where
  defaultValue =
    Autogen.SearchResponse2.Result.Result
      defaultValue
      defaultValue
      defaultValue
  rawType = RTLengthEncoded
  toRawValue msg = RLengthEncoded . runPut $ putRawMessage (toProto msg)
  fromRawValue (RLengthEncoded raw) = case runGet getRawMessage raw of
    Right rawMessage -> fromProto rawMessage
    Left err         -> Failure err
  fromRawValue _ = rawTypeFailure


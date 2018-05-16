{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Autogen.SearchResponse(module Data.Protobuf, module Autogen.SearchResponse) where

import Data.Protobuf
import Data.Protobuf.Encoding
import qualified Data.IntMap as IntMap
import GHC.Generics(Generic)

data SearchResponse = SearchResponse
  { query :: !Text
  , page_number :: !Int32
  , result_per_page :: !Int32
  , results :: ![Text]
  } deriving (Show, Generic)

instance ProtoMessage Autogen.SearchResponse.SearchResponse where
  fromProto raw =
    Autogen.SearchResponse.SearchResponse
      <$> raw .: 1
      <*> raw .: 2
      <*> raw .: 3
      <*> raw .: 5
  toProto Autogen.SearchResponse.SearchResponse{..} = RawMessage
    $ IntMap.empty
      & IntMap.insert 1 [toRawValue query]
      & IntMap.insert 2 [toRawValue page_number]
      & IntMap.insert 3 [toRawValue result_per_page]
      & IntMap.insert 5 [toRawValue results]

instance AsRawValue Autogen.SearchResponse.SearchResponse where
  defaultValue =
    Autogen.SearchResponse.SearchResponse
      defaultValue
      defaultValue
      defaultValue
      defaultValue
  rawType = RTLengthEncoded
  toRawValue msg = RLengthEncoded . runPut $ putRawMessage (toProto msg)
  fromRawValue (RLengthEncoded raw) = case runGet getRawMessage raw of
    Right rawMessage -> fromProto rawMessage
    Left err         -> Failure err
  fromRawValue _ = rawTypeFailure


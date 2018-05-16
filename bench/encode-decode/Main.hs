{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main(main) where

import Criterion.Main
import Data.ByteString(ByteString)

import qualified Data.Protobuf as My
import qualified Data.ProtoLens as Google

import qualified Autogen.SearchResponse as My.SR
import qualified Proto.Bench.SearchResponse as Google.SR

main :: IO ()
main = defaultMain
  [ bgroup "protoc"
      [ bgroup "SearchResponse" $ encodeDecode sampleMessage sampleMessageBytes
      ]
  , bgroup "hsprotoc"
      [ bgroup "SearchResponse" $ encodeDecode' sampleMessage' sampleMessageBytes'
      ]
  ]

myDecodeOrDie :: My.ProtoMessage m => ByteString -> m
myDecodeOrDie bytes = case My.decode bytes of
  Right a -> a
  Left _ -> error "My.decode failed"

-- Round trip encoding+decoding in both directions for Google's implementation
encodeDecode :: forall a. Google.Message a => a -> ByteString -> [Benchmark]
encodeDecode msg bytes =
    [ bench "Encode-Decode" $ whnf (Google.decodeMessage @a . Google.encodeMessage) msg
    , bench "Decode-Encode" $ nf (Google.encodeMessage . Google.decodeMessageOrDie @a) bytes
    ]

sampleMessage :: Google.SR.SearchResponse
sampleMessage = Google.SR.SearchResponse "some query" 1 2 ["result 1", "result 2", "result 3"]

sampleMessageBytes :: ByteString
sampleMessageBytes = Google.encodeMessage sampleMessage

-- Round trip encoding+decoding in both directions for this implementation
encodeDecode' :: forall a. My.ProtoMessage a => a -> ByteString -> [Benchmark]
encodeDecode' msg bytes =
    [ bench "Encode-Decode" $ whnf (myDecodeOrDie @a . My.encode) msg
    , bench "Decode-Encode" $ nf (My.encode @a . myDecodeOrDie) bytes
    ]

sampleMessage' :: My.SR.SearchResponse
sampleMessage' = My.SR.SearchResponse "some query" 1 2 ["result 1", "result 2", "result 3"]

sampleMessageBytes' :: ByteString
sampleMessageBytes' = My.encode sampleMessage'

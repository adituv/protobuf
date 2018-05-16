{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main(main) where

import Criterion.Main
import Data.ByteString(ByteString)
import Data.Monoid((<>))

import qualified Data.Protobuf as My
import qualified Data.ProtoLens as Google

import qualified Autogen.SearchResponse as My.SR
import qualified Proto.Bench.SearchResponse as Google.SR

import qualified Autogen.SearchResponse2 as My.SR2
import qualified Autogen.SearchResponse2.Result as My.SR2.R
import qualified Proto.Bench.SearchResponse2 as Google.SR2

import qualified Autogen.Test3 as My.Test3
import qualified Proto.Bench.Test3 as Google.Test3

main :: IO ()
main = defaultMain
  [ bgroup "protoc"
      [ bgroup "SearchResponse" $ encodeDecode sampleMessage sampleMessageBytes
      , bgroup "SearchResponse2" $ encodeDecode sampleMessage2 sampleMessage2Bytes
      ]
  , bgroup "hsprotoc"
      [ bgroup "SearchResponse" $ encodeDecode' sampleMessage' sampleMessageBytes'
      , bgroup "SearchResponse2" $ encodeDecode' sampleMessage2' sampleMessage2Bytes'
      ]
  ]

myDecodeOrDie :: My.ProtoMessage m => ByteString -> m
myDecodeOrDie bytes = case My.decode bytes of
  Right a -> a
  Left msg -> error $ "My.decode failed: " <> msg

-- Round trip encoding+decoding in both directions for Google's implementation
encodeDecode :: forall a. Google.Message a => a -> ByteString -> [Benchmark]
encodeDecode msg bytes =
    [ bench "Encode-Decode" $ whnf (Google.decodeMessage @a . Google.encodeMessage) msg
    , bench "Decode-Encode" $ nf (Google.encodeMessage . Google.decodeMessageOrDie @a) bytes
    ]

-- Round trip encoding+decoding in both directions for this implementation
encodeDecode' :: forall a. My.ProtoMessage a => a -> ByteString -> [Benchmark]
encodeDecode' msg bytes =
    [ bench "Encode-Decode" $ whnf (myDecodeOrDie @a . My.encode) msg
    , bench "Decode-Encode" $ nf (My.encode @a . myDecodeOrDie) bytes
    ]

sampleMessage :: Google.SR.SearchResponse
sampleMessage = Google.SR.SearchResponse "some query" 1 2 ["result 1", "result 2", "result 3"]

sampleMessageBytes :: ByteString
sampleMessageBytes = Google.encodeMessage sampleMessage

sampleMessage' :: My.SR.SearchResponse
sampleMessage' = My.SR.SearchResponse "some query" 1 2 ["result 1", "result 2", "result 3"]

sampleMessageBytes' :: ByteString
sampleMessageBytes' = My.encode sampleMessage'

sampleMessage2 :: Google.SR2.SearchResponse2
sampleMessage2 =
    Google.SR2.SearchResponse2
      [ Google.SR2.SearchResponse2'Result
          "someurl"
          "sometitle"
          ["snippet1", "snippet2", "snippet3"]
      , Google.SR2.SearchResponse2'Result
          "someurl"
          "sometitle"
          ["snippet1", "snippet2", "snippet3"]
      ]

sampleMessage2Bytes :: ByteString
sampleMessage2Bytes = Google.encodeMessage sampleMessage2

sampleMessage2' :: My.SR2.SearchResponse2
sampleMessage2' =
    My.SR2.SearchResponse2
      [ My.SR2.R.Result
          "someurl"
          "sometitle"
          ["snippet1", "snippet2", "snippet3"]
      , My.SR2.R.Result
          "someurl"
          "sometitle"
          ["snippet1", "snippet2", "snippet3"]
      ]

sampleMessage2Bytes' :: ByteString
sampleMessage2Bytes' = My.encode sampleMessage2'

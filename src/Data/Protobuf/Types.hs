{-# LANGUAGE PatternSynonyms #-}
module Data.Protobuf.Types where

import Data.ByteString(ByteString)
import Data.Int(Int32, Int64)
import Data.Map.Strict(Map)
import Data.Word(Word8, Word32, Word64)


newtype RawMessage = RawMessage (Map Word32 [RawValue])

data RawValue = RVarInt Integer
              | RLengthEncoded ByteString
              | RFixed32 Word32
              | RFixed64 Word64
              deriving Show

pattern TVarInt :: Word8
pattern TVarInt = 0

pattern TLengthEncoded :: Word8
pattern TLengthEncoded = 2

pattern TFixed32 :: Word8
pattern TFixed32 = 5

pattern TFixed64 :: Word8
pattern TFixed64 = 1

newtype Fixed32 = Fixed32 Word32
newtype Fixed64 = Fixed64 Word64
newtype SInt32 = SInt32 Int32
newtype SInt64 = SInt64 Int64

data Result a = Failure String | Success a deriving Show

instance Functor Result where
  fmap f (Success a) = Success (f a)
  fmap _ (Failure f) = Failure f

instance Applicative Result where
  pure = Success
  (Success f) <*> (Success x) = Success (f x)
  (Failure f) <*> _ = Failure f
  _           <*> (Failure x) = Failure x

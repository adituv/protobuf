{-# LANGUAGE PatternSynonyms #-}
module Data.Protobuf.Types where

import Data.ByteString(ByteString)
import Data.Map.Strict(Map)
import Data.Word(Word8, Word32, Word64)

newtype RawMessage = RawMessage (Map Word32 [RawValue])

data RawValue = VarInt Integer
              | LengthEncoded ByteString
              | Fixed32 Word32
              | Fixed64 Word64
              deriving Show

pattern TVarInt :: Word8
pattern TVarInt = 0

pattern TLengthEncoded :: Word8
pattern TLengthEncoded = 2

pattern TFixed32 :: Word8
pattern TFixed32 = 5

pattern TFixed64 :: Word8
pattern TFixed64 = 1

data Result a = Failure String | Success a deriving Show

instance Functor Result where
  fmap f (Success a) = Success (f a)
  fmap _ (Failure f) = Failure f

instance Applicative Result where
  pure = Success
  (Success f) <*> (Success x) = Success (f x)
  (Failure f) <*> _ = Failure f
  _           <*> (Failure x) = Failure x
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns      #-}
module Data.Protobuf.Encoding where

import Data.Protobuf.Types

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Map.Strict as Map

import Control.Monad.IfElse(untilM)
import Control.Monad.State.Strict
import Data.Bits
import Data.ByteString(ByteString)
import Data.ByteString.Builder(Builder)
import Data.Int(Int32, Int64)
import Data.Serialize hiding (Result)
import Data.Word(Word8, Word32, Word64)

import GHC.Integer.GMP.Internals
import GHC.Types


maxTag :: Integer
maxTag = 0x1FFFFFFF -- 2^29 - 1


-- * Getters


-- Will always be parsed as positive - conversion function has to handle
-- possible negative values
getVarInt :: Get Integer
getVarInt = do
  x <- getWord8
  case x of
    HasContinuation y -> do
      rest <- getVarInt
      pure $ (rest `shiftL` 7) .|. toInteger y
    _ -> pure $ toInteger x

getLengthEncoded :: Get ByteString
getLengthEncoded = do
  len <- fromInteger <$> getVarInt
  isolate len $ getByteString len

getRawMessage :: Get RawMessage
getRawMessage = fmap RawMessage
             <$> flip execStateT Map.empty
              $  untilM (lift isEmpty) $ do
                   (t,v) <- lift getRawMessageComponent
                   when (t > maxTag) $ fail ("Tag value out of range: " ++ show t)
                   modify' $ Map.insertWith (++) (fromIntegral t) [v]


getRawMessageComponent :: Get (Integer, RawValue)
getRawMessageComponent = do
  (ProtoMeta tag typ) <- getVarInt
  value <- case typ of
    TVarInt -> RVarInt <$> getVarInt
    TLengthEncoded -> RLengthEncoded <$> getLengthEncoded
    TFixed32 -> RFixed32 <$> getWord32le
    TFixed64 -> RFixed64 <$> getWord64le
    _      -> fail $ "Unsupported or invalid protobuf type: " ++ show typ
  pure (tag, value)

-- * Putters

putProtoMeta :: Word32 -> Word8 -> Put
putProtoMeta tag typ = putVarInt $ fromIntegral (tag `shiftL` 3 .|. fromIntegral typ)

putVarInt :: Putter Integer
putVarInt (Jn# _) = fail "Negative numbers outside the bounds of Int64 not currently supported"
putVarInt x       = putByteString . toVarIntBytes $ x 

toVarIntBytes :: Integer -> ByteString
toVarIntBytes 0 = "\0"
toVarIntBytes x = Lazy.toStrict
                . Builder.toLazyByteString
                . toVarIntBytes'
                $ x

toVarIntBytes' :: Integer -> Builder
toVarIntBytes' 0 = mempty
toVarIntBytes' x = let (next, rest) = splitVarIntByte x
                   in  Builder.word8 next `mappend` toVarIntBytes' rest

splitVarIntByte :: Integer -> (Word8, Integer)
splitVarIntByte x =
  let next = x .&. 0x7F
      rest = x `lshiftR` 7
  in  (fromIntegral next, rest)

lshiftR :: Integer -> Int -> Integer
lshiftR x n = case x of
  S# x' -> fromIntegral $ I# x' `shiftR` n .&. ((1 `shiftL` (finiteBitSize (I# x') - n)) - 1)
  Jp# _ -> x `shiftR` n
  Jn# _ -> error "Precondition violated: lshiftR does not take Jn#"

putLengthEncoded :: Putter ByteString
putLengthEncoded bs = do
  let len = ByteString.length bs
  putVarInt $ fromIntegral len
  putByteString bs

putRawMessage :: Putter RawMessage
putRawMessage (RawMessage msg) = mapM_ putRawMessageComponent
                               $ Map.toAscList msg

putRawMessageComponent :: Putter (Word32, [RawValue])
putRawMessageComponent (t, vs) = mapM_ putSingleValue vs
  where
    putSingleValue v = case v of
      RVarInt x -> putProtoMeta t TVarInt >> putVarInt x
      RLengthEncoded bs -> putProtoMeta t TLengthEncoded >> putLengthEncoded bs
      RFixed32 x -> putProtoMeta t TFixed32 >> putWord32le x
      RFixed64 x -> putProtoMeta t TFixed64 >> putWord64le x


-- * Convenience patterns

pattern HasContinuation :: Word8 -> Word8
pattern HasContinuation y <- (varIntByte -> Right y)

varIntByte :: Word8 -> Either Word8 Word8
varIntByte x
  | x .&. 0x80 /= 0  = Right (x .&. 0x7F)
  | otherwise = Left x

pattern ProtoMeta :: Integer -> Word8 -> Integer
pattern ProtoMeta tag typ <- (splitProtoMeta -> (tag,typ))

splitProtoMeta :: Integer -> (Integer, Word8)
splitProtoMeta x = (x `div` 8, fromIntegral $ x .&. 7)

-- * Conversion from 'RawValue's

class AsRawValue a where
    defaultValue :: a
    toRawValue :: a -> RawValue
    fromRawValue :: RawValue -> Result a

instance AsRawValue Int32 where
    defaultValue = 0
    toRawValue x = RVarInt $ toInteger (fromIntegral x :: Word32)
    fromRawValue (RVarInt x)
      | x >= toInteger (minBound @Word32) && x <= toInteger (maxBound @Word32) = Success $ fromInteger x
      | otherwise = Failure "Value out of range"
    fromRawValue _ = Failure "Incompatible raw value type"

instance AsRawValue Int64 where
    defaultValue = 0
    toRawValue x = RVarInt $ toInteger (fromIntegral x :: Word64)
    fromRawValue (RVarInt x)
      | x >= toInteger (minBound @Word64) && x <= toInteger (maxBound @Word64) = Success $ fromInteger x
      | otherwise = Failure "Value out of range"
    fromRawValue _ = Failure "Incompatible raw value type"

instance AsRawValue Word32 where
    defaultValue = 0
    toRawValue x = RVarInt $ toInteger x
    fromRawValue (RVarInt x)
      | x >= toInteger (minBound @Word32) && x <= toInteger (maxBound @Word32) = Success $ fromInteger x
      | otherwise = Failure "Value out of range"
    fromRawValue _ = Failure "Incompatible raw value type"

instance AsRawValue Word64 where
    defaultValue = 0
    toRawValue x = RVarInt $ toInteger x
    fromRawValue (RVarInt x)
      | x >= toInteger (minBound @Word64) && x <= toInteger (maxBound @Word64) = Success $ fromInteger x
      | otherwise = Failure "Value out of range"
    fromRawValue _ = Failure "Incompatible raw value type"

instance AsRawValue Fixed32 where
    defaultValue = Fixed32 0
    toRawValue (Fixed32 x) = RFixed32 x
    fromRawValue (RFixed32 x) = Success $ Fixed32 x
    fromRawValue _ = Failure "Incompatible raw value type"

instance AsRawValue Fixed64 where
    defaultValue = Fixed64 0
    toRawValue (Fixed64 x) = RFixed64 x
    fromRawValue (RFixed64 x) = Success $ Fixed64 x
    fromRawValue _ = Failure "Incompatible raw value type"

-- TODO Signed integer encodings
-- TODO Text
-- TODO Nested messages
-- TODO Lists

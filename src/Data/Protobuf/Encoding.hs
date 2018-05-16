{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns      #-}
module Data.Protobuf.Encoding(module Data.Protobuf.Types, module Data.Protobuf.Encoding, module Data.Serialize) where

import Data.Protobuf.Types

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map

import Control.Applicative(many)
import Control.Exception(displayException)
import Control.Monad.IfElse(untilM)
import Control.Monad.State.Strict
import Data.Bits
import Data.ByteString(ByteString)
import Data.ByteString.Builder(Builder)
import Data.Int(Int32, Int64)
import Data.Serialize(Get, Put, Putter, getWord8, getWord32le, getWord64le, getByteString, isolate, putWord32le, putWord64le, runPut, runGet, isEmpty, putByteString)
import Data.Text(Text)
import Data.Text.Encoding(decodeUtf8', encodeUtf8)
import Data.Word(Word8, Word32, Word64)


maxTag :: Integer
maxTag = 0x1FFFFFFF -- 2^29 - 1

rawTypeFailure :: Result a
rawTypeFailure = Failure "Incompatible raw value type"

valueRangeFailure :: Result a
valueRangeFailure = Failure "Value out of range"

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
             <$> flip execStateT IntMap.empty
              $  untilM (lift isEmpty) $ do
                   (t,v) <- lift getRawMessageComponent
                   when (t > maxTag) $ fail ("Tag value out of range: " ++ show t)
                   modify' $ IntMap.insertWith (++) (fromIntegral t) [v]


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

putProtoMeta :: Int -> Word8 -> Put
putProtoMeta tag typ = putVarInt $ fromIntegral (tag `shiftL` 3 .|. fromIntegral typ)

putVarInt :: Putter Integer
putVarInt = putByteString . toVarIntBytes

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
      rest = x `shiftR` 7 -- arithmetic shift but x never negative during varint decoding
  in  (fromIntegral next, rest)

putLengthEncoded :: Putter ByteString
putLengthEncoded bs = do
  let len = ByteString.length bs
  putVarInt $ fromIntegral len
  putByteString bs

putRawMessage :: Putter RawMessage
putRawMessage (RawMessage msg) = mapM_ putRawMessageComponent $ IntMap.toAscList msg

putRawMessageComponent :: Putter (Int, [RawValue])
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
    rawType :: RawType
    toRawValue :: a -> RawValue
    fromRawValue :: RawValue -> Result a

data RawType = RTVarInt | RTLengthEncoded | RTFixed32 | RTFixed64 deriving (Show, Eq)

instance AsRawValue Int32 where
    defaultValue = 0
    rawType = RTVarInt
    toRawValue x = RVarInt $ toInteger (fromIntegral x :: Word32)
    fromRawValue (RVarInt x)
      | x >= toInteger (minBound @Word32) && x <= toInteger (maxBound @Word32) = Success $ fromInteger x
      | otherwise = valueRangeFailure
    fromRawValue _ = rawTypeFailure

instance AsRawValue Int64 where
    defaultValue = 0
    rawType = RTVarInt
    toRawValue x = RVarInt $ toInteger (fromIntegral x :: Word64)
    fromRawValue (RVarInt x)
      | x >= toInteger (minBound @Word64) && x <= toInteger (maxBound @Word64) = Success $ fromInteger x
      | otherwise = valueRangeFailure
    fromRawValue _ = rawTypeFailure

instance AsRawValue Word32 where
    defaultValue = 0
    rawType = RTVarInt
    toRawValue x = RVarInt $ toInteger x
    fromRawValue (RVarInt x)
      | x >= toInteger (minBound @Word32) && x <= toInteger (maxBound @Word32) = Success $ fromInteger x
      | otherwise = valueRangeFailure
    fromRawValue _ = rawTypeFailure

instance AsRawValue Word64 where
    defaultValue = 0
    rawType = RTVarInt
    toRawValue x = RVarInt $ toInteger x
    fromRawValue (RVarInt x)
      | x >= toInteger (minBound @Word64) && x <= toInteger (maxBound @Word64) = Success $ fromInteger x
      | otherwise = valueRangeFailure
    fromRawValue _ = rawTypeFailure

instance AsRawValue Fixed32 where
    defaultValue = Fixed32 0
    rawType = RTFixed32
    toRawValue (Fixed32 x) = RFixed32 x
    fromRawValue (RFixed32 x) = Success $ Fixed32 x
    fromRawValue _ = rawTypeFailure

instance AsRawValue Fixed64 where
    defaultValue = Fixed64 0
    rawType = RTFixed64
    toRawValue (Fixed64 x) = RFixed64 x
    fromRawValue (RFixed64 x) = Success $ Fixed64 x
    fromRawValue _ = rawTypeFailure

class (Integral a, Integral b) => SignConv a b | a -> b, b -> a where
  toSigned :: a -> b
  toSigned = fromIntegral

  toUnsigned :: b -> a
  toUnsigned = fromIntegral

instance SignConv Word32 Int32
instance SignConv Word64 Int64

zigzag :: FiniteBits a => a -> a
zigzag x = (x `shiftL` 1) `xor` (x `shiftR` (finiteBitSize x - 1))

unzigzag :: (SignConv b a, Num a, Bits a, Bits b) => a -> a
unzigzag x = toSigned (x' `shiftR` 1) `xor` (-x .&. 1)
  where
    x' = toUnsigned x

instance AsRawValue SInt32 where
    defaultValue = SInt32 0
    rawType = RTVarInt
    toRawValue (SInt32 x) = RVarInt (fromIntegral $ zigzag x)
    fromRawValue (RVarInt x)
      | x <= fromIntegral (maxBound @Word32) = Success $ SInt32 (unzigzag $ fromIntegral x)
      | otherwise = valueRangeFailure
    fromRawValue _ = rawTypeFailure

instance AsRawValue SInt64 where
    defaultValue = SInt64 0
    rawType = RTVarInt
    toRawValue (SInt64 x) = RVarInt (fromIntegral $ zigzag x)
    fromRawValue (RVarInt x)
      | x <= fromIntegral (maxBound @Word32) = Success $ SInt64 (unzigzag $ fromIntegral x)
      | otherwise = valueRangeFailure
    fromRawValue _ = rawTypeFailure

instance AsRawValue Text where
    defaultValue = ""
    rawType = RTLengthEncoded
    toRawValue txt = RLengthEncoded $ encodeUtf8 txt
    fromRawValue (RLengthEncoded bs) =
        case decodeUtf8' bs of
            Right txt -> Success txt
            Left err -> Failure $ displayException err
    fromRawValue _ = rawTypeFailure

instance AsRawValue Bool where
    defaultValue = False
    rawType = RTVarInt
    toRawValue True = RVarInt 1
    toRawValue False = RVarInt 0
    fromRawValue (RVarInt 0) = Success False
    fromRawValue (RVarInt _) = Success True
    fromRawValue _ = rawTypeFailure

-- Nested messages will be handled in their generated source files

instance AsRawValue a => AsRawValue [a] where
    defaultValue = []
    rawType = RTLengthEncoded
    toRawValue xs = RLengthEncoded . runPut $ mapM_ (putValueNoTag . toRawValue) xs
      where
        putValueNoTag (RVarInt x) = putVarInt x
        putValueNoTag (RLengthEncoded x) = putLengthEncoded x
        putValueNoTag (RFixed32 x) = putWord32le x
        putValueNoTag (RFixed64 x) = putWord64le x

    fromRawValue (RLengthEncoded bs) =
        case runGet (listGetterFor $ rawType @a) bs of
            Left e -> Failure e
            Right x -> traverse fromRawValue x
    fromRawValue _ = rawTypeFailure

listGetterFor :: RawType -> Get [RawValue]
listGetterFor RTVarInt = many (RVarInt <$> getVarInt)
listGetterFor RTLengthEncoded = many (RLengthEncoded <$> getLengthEncoded)
listGetterFor RTFixed32 = many (RFixed32 <$> getWord32le)
listGetterFor RTFixed64 = many (RFixed64 <$> getWord64le)

(.:) :: AsRawValue a => RawMessage -> Int -> Result a
(RawMessage msg) .: tag = case IntMap.lookup tag msg of
    Just (x:_) -> fromRawValue x
    _          -> pure defaultValue

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
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
import Data.Serialize
import Data.Word(Word8, Word32)


maxTag :: Integer
maxTag = 0x1FFFFFFF -- 2^29 - 1


-- * Getters

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
    TVarInt -> VarInt <$> getVarInt
    TLengthEncoded -> LengthEncoded <$> getLengthEncoded
    TFixed32 -> Fixed32 <$> getWord32le
    TFixed64 -> Fixed64 <$> getWord64le
    _      -> fail $ "Unsupported or invalid protobuf type: " ++ show typ
  pure (tag, value)

-- * Putters

putProtoMeta :: Word32 -> Word8 -> Put
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
      rest = x `div` 128
  in  (fromIntegral next, rest)

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
      VarInt x -> putProtoMeta t TVarInt >> putVarInt x
      LengthEncoded bs -> putProtoMeta t TLengthEncoded >> putLengthEncoded bs
      Fixed32 x -> putProtoMeta t TFixed32 >> putWord32le x
      Fixed64 x -> putProtoMeta t TFixed64 >> putWord64le x


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
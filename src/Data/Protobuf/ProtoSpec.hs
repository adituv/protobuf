module Data.Protobuf.ProtoSpec( ProtoSpec(..)
                              , FieldSpec(..)
                              , FieldModifier(..)
                              , ProtoType(..)
                              , Reservation(..)
                              ) where

-- | The specification of a protobuf message, generally located
--   in a ".proto" file.
data ProtoSpec = ProtoSpec
  { messageName :: String     -- ^ The name of the message type
  , innerSpecs :: [ProtoSpec] -- ^ Specifications for nested message types
  , fields :: [FieldSpec]         -- ^ The fields specified by the specification
  , reserved :: [Reservation] -- ^ Reserved tags that must not be used
  } deriving Show

-- | A value field in the specification of a protobuf message.
data FieldSpec = FieldSpec
  { fieldMod :: !FieldModifier -- ^ The field's modifier.  In proto3, effectively
                              --   equivalent to whether the field is repeated
  , fieldType :: !ProtoType -- ^ The type of the field
  , fieldName :: String -- ^ The name of the field
  , fieldTag :: !Int -- ^ The relative position of the field within the
                     --   message
  } deriving Show

-- | The field's modifier.  In proto2, there was also a "required" modifier,
--   meaning the field must be present, and also an "optional" modifier.  In
--   proto3, there is no longer a "required" modifier, and "optional" is 
--   represented by a lack of a modifier.
data FieldModifier = Optional | Repeated deriving (Show, Eq)

-- | The possible types of fields in a protobuf message
data ProtoType = PDouble -- ^ 64-bit IEEE754 floating point
               | PFloat  -- ^ 32-bit IEEE754 floating point
               | PInt32  -- ^ Signed 32-bit integer, variable-width encoding;
                         --   inefficient for negative numbers.
               | PInt64  -- ^ Signed 64-bit integer, variable-width encoding;
                         --   inefficient for negative numbers.
               | PUInt32 -- ^ Unsigned 32-bit integer, variable-width encoding.
               | PUInt64 -- ^ Unsigned 64-bit integer, variable-width encoding.
               | PSInt32 -- ^ Signed variable-width integer, max 2^31 - 1.
                         --   More efficient encoding than `PInt32` for negative 
                         --   integers.
               | PSInt64 -- ^ Signed variable-width integer, max 2^32 - 1.
                         --   More efficient encodingthan `PInt64` for negative
                         --   integers.
               | PFixed32 -- ^ Unsigned 32-bit integer, fixed-width encoding.
               | PFixed64 -- ^ Unsigned 64-bit integer, fixed-width encoding.
               | PSFixed32 -- ^ Signed 32-bit integer, fixed-width encoding.
               | PSFixed64 -- ^ Signed 64-bit integer, fixed-width encoding
               | PBool -- ^ Bool
               | PString -- ^ String 
               | PBytes -- ^ ByteString
               | NamedField String -- ^ A message or enum type.
               deriving (Show, Eq)

-- | The specification of a tag or name that may not be used in a protobuf
--   message
data Reservation = RTag !Int -- ^ A single tag
                 | RTagToMax !Int -- ^ All tags greater than or equal to an int
                 | RTagRange !Int !Int -- ^ All tags in a range, inclusive
                 | RName String -- ^ A single reserved name
                 deriving Show
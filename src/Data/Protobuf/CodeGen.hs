{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Protobuf.CodeGen where

import           Data.Protobuf.ProtoSpec

import           Data.Char               (toUpper)
import qualified Data.HashMap.Lazy       as HashMap
import           Data.Monoid             (mconcat, (<>))
import           Data.String             (IsString (..))
import qualified Data.Text.Lazy          as Text.Lazy
import qualified Data.Text.Lazy.Builder  as Text.Builder

type LazyText = Text.Lazy.Text
type TextBuilder = Text.Builder.Builder

genFiles :: LazyText -> ProtoSpec -> [(FilePath, LazyText)]
genFiles scope spec = HashMap.toList $ genFilesAux scope' spec
  where
    scope' = Text.Lazy.intercalate "."
           . fmap capitalizeT
           . Text.Lazy.splitOn "."
           $ scope

    capitalizeT :: LazyText -> LazyText
    capitalizeT txt = case Text.Lazy.uncons txt of
      Nothing    -> ""
      Just (h,t) -> Text.Lazy.cons (toUpper h) t

genFilesAux :: LazyText -> ProtoSpec -> HashMap.HashMap FilePath LazyText
genFilesAux scope spec@ProtoSpec{..} =
    HashMap.insert fileName (genFile scope spec)
  $ HashMap.unions (genFilesAux innerScope <$> innerSpecs)
  where
    innerScope = Text.Builder.toLazyText
               $ addScope scope (fromString $ capitalize messageName)
    fileName = Text.Lazy.unpack $
               Text.Lazy.replace "." "/" innerScope <> ".hs"

genFile :: LazyText -> ProtoSpec -> LazyText
genFile scope = Text.Builder.toLazyText . genSpec scope

endl :: TextBuilder
endl = "\n"

genSpec :: LazyText -> ProtoSpec -> TextBuilder
genSpec scope ProtoSpec{..} =
       "module " <> scopedName <> "(module Data.Protobuf, module " <> scopedName <> ") where" <> endl
    <> endl
    <> "import Data.Protobuf" <> endl
    <> "import Data.Protobuf.Encoding" <> endl
    <> "import GHC.Generics(Generic)" <> endl
    <> mconcat (genImport scopedName <$> innerSpecs)
    <> endl
    <> "data " <> datatypeName <> " = " <> datatypeName <> endl
    <> genFields fields
    <> " deriving (Show, Generic)" <> endl <> endl
    <> genInstances scopedName fields
    <> endl
  where
    datatypeName = fromString $ capitalize messageName
    scopedName = addScope scope datatypeName

addScope :: LazyText -> TextBuilder -> TextBuilder
addScope scope name
  | Text.Lazy.null scope = name
  | otherwise = Text.Builder.fromLazyText scope <> "." <> name

genImport :: TextBuilder -> ProtoSpec -> TextBuilder
genImport scope ProtoSpec{..} =
  "import " <> scope <> "." <> fromString messageName <> endl

genInstances :: TextBuilder -> [FieldSpec] -> TextBuilder
genInstances scopedName fields =
    protoMessageInstance <> endl <> asRawValueInstance <> endl
  where
    protoMessageInstance =
      "instance ProtoMessage " <> scopedName <> " where" <> endl <>
      "  fromProto raw =" <> endl <>
      "    " <> scopedName <> endl <>
      "      <$> " <>
      intercalate (endl <> "      <*> ") (genFieldFromProto <$> fields) <> endl <>
      "  toProto scopedName{..} = RawMessage" <> endl <>
      "    $ Map.empty" <> endl <>
      foldMap genFieldToProto fields

    asRawValueInstance =
      "instance AsRawValue " <> scopedName <> " where" <> endl <>
      "  defaultValue = " <> endl <>
      "    " <> scopedName <> endl <>
      "      <$> " <>
      intercalate (endl <> "      <*> ") ("defaultValue" <$ fields) <> endl <>
      "  rawType = RTLengthEncoded" <> endl <>
      "  toRawValue msg = RLengthEncoded $ runPut putRawMessage (toProto msg)" <> endl <>
      "  fromRawValue (RLengthEncoded raw) = error \"TODO\""


genFieldFromProto :: FieldSpec -> TextBuilder
genFieldFromProto FieldSpec{..} = "raw .: " <> fromString (show fieldTag)

genFieldToProto :: FieldSpec -> TextBuilder
genFieldToProto FieldSpec{..} = "      & Map.insert " <> fromString (show fieldTag) <> "(toRaw " <> fromString fieldName <> ")" <> endl

genFields :: [FieldSpec] -> TextBuilder
genFields fields =
  "  { " <> intercalate (endl <> "  , ") (genField <$> fields)
         <> endl <> "  }"

genField :: FieldSpec -> TextBuilder
genField FieldSpec{..} =
    fromString fieldName <> " :: " <> fieldType'
  where
    fieldType' = case fieldMod of
      Optional -> genType fieldType
      Repeated -> "[" <> genType fieldType <> "]"

genType :: ProtoType -> TextBuilder
genType PDouble        = "Double"
genType PFloat         = "Float"
genType PInt32         = "Int32"
genType PInt64         = "Int64"
genType PUInt32        = "Word32"
genType PUInt64        = "Word64"
genType PSInt32        = "Int32"
genType PSInt64        = "Int64"
genType PFixed32       = "Word32"
genType PFixed64       = "Word64"
genType PSFixed32      = "Int32"
genType PSFixed64      = "Int64"
genType PBool          = "Bool"
genType PString        = "Text"
genType PBytes         = "ByteString"
genType (NamedField s) = fromString $ capitalize s

intercalate :: TextBuilder -> [TextBuilder] -> TextBuilder
intercalate _   []     = mempty
intercalate _   [x]    = x
intercalate mid (x:xs) = x <> mid <> intercalate mid xs

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs

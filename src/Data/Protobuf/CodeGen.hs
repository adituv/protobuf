{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Protobuf.CodeGen where

import           Data.Protobuf.ProtoSpec

import           Data.Char               (toUpper)
import           Data.Monoid             (mconcat, (<>))
import           Data.String             (IsString (..))
import qualified Data.Text.Lazy          as Text.Lazy
import qualified Data.Text.Lazy.Builder  as Text.Builder

type LazyText = Text.Lazy.Text
type TextBuilder = Text.Builder.Builder

genFile :: ProtoSpec -> LazyText
genFile = Text.Builder.toLazyText . genSpec ""

endl :: TextBuilder
endl = "\n"

genSpec :: LazyText -> ProtoSpec -> TextBuilder
genSpec scope ProtoSpec{..} =
       "module " <> scopedName <> " where" <> endl
    <> endl
    <> "import Data.Protobuf" <> endl
    <> mconcat (genImport scopedName <$> innerSpecs)
    <> endl
    <> "data " <> datatypeName <> " = " <> datatypeName <> endl
    <> genFields fields
    <> endl
  where
    messageName' = fromString messageName
    datatypeName = fromString $ case messageName of
      c:cs -> toUpper c : cs
      []   -> []
    scopedName = addScope scope messageName'

addScope :: LazyText -> TextBuilder -> TextBuilder
addScope scope name
  | Text.Lazy.null scope = name
  | otherwise = Text.Builder.fromLazyText scope <> "." <> name

genImport :: TextBuilder -> ProtoSpec -> TextBuilder
genImport scope ProtoSpec{..} =
  "import " <> scope <> "." <> fromString messageName <> endl

genFields :: [FieldSpec] -> TextBuilder
genFields fields =
  "  { " <> intercalate (endl <> "  , ") (genField <$> fields)
         <> endl <> "  }" <> endl

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
genType (NamedField s) = fromString s

intercalate :: TextBuilder -> [TextBuilder] -> TextBuilder
intercalate _   []     = mempty
intercalate _   [x]    = x
intercalate mid (x:xs) = x <> mid <> intercalate mid xs

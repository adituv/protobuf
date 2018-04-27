{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Protobuf.CodeGen where

import           Data.Protobuf.ProtoSpec

import           Control.Arrow           ((&&&))
import           Data.Char               (toUpper)
import qualified Data.HashMap.Lazy       as HashMap
import           Data.Monoid             (mconcat, (<>))
import           Data.String             (IsString (..))
import qualified Data.Text.Lazy          as Text.Lazy
import qualified Data.Text.Lazy.Builder  as Text.Builder
import           Data.Tree.ScopeTree     (ScopeTree)
import qualified Data.Tree.ScopeTree     as ScopeTree

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
genFile scope spec = Text.Builder.toLazyText . genSpec scope (collectNames spec) $ spec

collectNames :: [LazyText] -> ProtoSpec -> ScopeTree LazyText
collectNames scope ProtoSpec{..} = ScopeTree.insert messageScoped
                                 $ mconcat (collectNames messageScoped <$> innerSpecs)
  where
    messageScoped = scope ++ [Text.Lazy.pack messageName]

endl :: TextBuilder
endl = "\n"

genSpec :: LazyText -> ScopeTree LazyText -> ProtoSpec -> TextBuilder
genSpec scope namesInScope ProtoSpec{..} =
       "{-# LANGUAGE RecordWildCards #-}" <> endl
    <> "module " <> scopedName <> "(module Data.Protobuf, module " <> scopedName <> ") where" <> endl
    <> endl
    <> "import Data.Protobuf" <> endl
    <> "import Data.Protobuf.Encoding" <> endl
    <> "import GHC.Generics(Generic)" <> endl
    <> mconcat (genImport scopedName <$> innerSpecs)
    <> endl
    <> "data " <> datatypeName <> " = " <> datatypeName <> endl
    <> genFields namesInScope fields
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
          "  toProto " <> scopedName <> "{..} = RawMessage" <> endl <>
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

genFields :: [FieldSpec] -> ScopeTree LazyText -> TextBuilder
genFields fields namesInScope =
  "  { " <> intercalate (endl <> "  , ") (genField namesInScope <$> fields)
         <> endl <> "  }"

genField :: FieldSpec -> ScopeTree LazyText -> TextBuilder
genField FieldSpec{..} namesInScope =
    fromString fieldName <> " :: " <> fieldType'
  where
    fieldType' = case fieldMod of
      Optional -> genType namesInScope fieldType
      Repeated -> "[" <> genType nameInScope fieldType <> "]"

genType :: [LazyText] -> ScopeTree LazyText -> ProtoType -> Maybe TextBuilder
genType _ _ PDouble        = Just "Double"
genType _ _ PFloat         = Just "Float"
genType _ _ PInt32         = Just "Int32"
genType _ _ PInt64         = Just "Int64"
genType _ _ PUInt32        = Just "Word32"
genType _ _ PUInt64        = Just "Word64"
genType _ _ PSInt32        = Just "Int32"
genType _ _ PSInt64        = Just "Int64"
genType _ _ PFixed32       = Just "Word32"
genType _ _ PFixed64       = Just "Word64"
genType _ _ PSFixed32      = Just "Int32"
genType _ _ PSFixed64      = Just "Int64"
genType _ _ PBool          = Just "Bool"
genType _ _ PString        = Just "Text"
genType _ _ PBytes         = Just "ByteString"
genType scope names (NamedField s)
  | s `startsWith` "."     = Just $ normalizeType (tail s)
  | otherwise              = resolveName s
  where
    possibleScopes = reverse $ inits scope
    possibleScopedNames = fmap (++ [name]) possibleScopes
    resolveName name = find (ScopeTree.contains names) possibleScopedNames

intercalate :: TextBuilder -> [TextBuilder] -> TextBuilder
intercalate _   []     = mempty
intercalate _   [x]    = x
intercalate mid (x:xs) = x <> mid <> intercalate mid xs

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs

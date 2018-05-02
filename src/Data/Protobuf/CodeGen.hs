{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Protobuf.CodeGen where

import           Data.Protobuf.ProtoSpec

import           Control.Arrow           ((&&&))
import           Data.Char               (toUpper)
import qualified Data.HashMap.Lazy       as HashMap
import qualified Data.List               as List
import           Data.Monoid             (mconcat, (<>))
import           Data.String             (IsString (..))
import qualified Data.Text.Lazy          as Text.Lazy
import qualified Data.Text.Lazy.Builder  as Text.Builder
import           Data.Tree.ScopeTree     (ScopeTree)
import qualified Data.Tree.ScopeTree     as ScopeTree

type LazyText = Text.Lazy.Text
type TextBuilder = Text.Builder.Builder

genFiles :: LazyText -> ProtoSpec -> Maybe [(FilePath, LazyText)]
genFiles scope spec = HashMap.toList <$> genFilesAux scope' spec
  where
    scope' = Text.Lazy.intercalate "."
           . fmap capitalizeT
           . Text.Lazy.splitOn "."
           $ scope

    capitalizeT :: LazyText -> LazyText
    capitalizeT txt = case Text.Lazy.uncons txt of
      Nothing    -> ""
      Just (h,t) -> Text.Lazy.cons (toUpper h) t

genFilesAux :: LazyText -> ProtoSpec -> Maybe (HashMap.HashMap FilePath LazyText)
genFilesAux scope spec@ProtoSpec{..} = do
    childFiles <- mapM (genFilesAux innerScope) innerSpecs
    thisFile <- genFile scope spec
    pure $ HashMap.insert fileName thisFile (HashMap.unions childFiles)
  where
    innerScope = Text.Builder.toLazyText
               $ addScope scope (fromString $ capitalize messageName)
    fileName = Text.Lazy.unpack $
               Text.Lazy.replace "." "/" innerScope <> ".hs"

genFile :: LazyText -> ProtoSpec -> Maybe LazyText
genFile scope spec = Text.Builder.toLazyText <$> genSpec scope (collectNames currentScope spec) spec
  where
    currentScope = Text.Lazy.splitOn "." scope

collectNames :: [LazyText] -> ProtoSpec -> ScopeTree LazyText
collectNames scope ProtoSpec{..} = ScopeTree.insert messageScoped
                                 $ mconcat (collectNames messageScoped <$> innerSpecs)
  where
    messageScoped = scope ++ [Text.Lazy.pack messageName]

endl :: TextBuilder
endl = "\n"

genSpec :: LazyText -> ScopeTree LazyText -> ProtoSpec -> Maybe TextBuilder
genSpec scope namesInScope ProtoSpec{..} = do
  fieldDefs <- genFields currentScope namesInScope fields
  pure $ "{-# LANGUAGE RecordWildCards #-}" <> endl
      <> "module " <> scopedName <> "(module Data.Protobuf, module " <> scopedName <> ") where" <> endl
      <> endl
      <> "import Data.Protobuf" <> endl
      <> "import Data.Protobuf.Encoding" <> endl
      <> "import GHC.Generics(Generic)" <> endl
      <> mconcat (genImport scopedName <$> innerSpecs)
      <> endl
      <> "data " <> datatypeName <> " = " <> datatypeName <> endl
      <> fieldDefs
      <> " deriving (Show, Generic)" <> endl <> endl
      <> genInstances scopedName fields
      <> endl
  where
    datatypeName = fromString $ capitalize messageName
    scopedName = addScope scope datatypeName
    currentScope = Text.Lazy.splitOn "." scope

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

genFields :: [LazyText] -> ScopeTree LazyText -> [FieldSpec] -> Maybe TextBuilder
genFields currentScope namesInScope fields= do
  fieldSpecs <- mapM (genField currentScope namesInScope) fields
  pure $ "  { " <> intercalate (endl <> "  , ") fieldSpecs <> endl <> "  }"

genField :: [LazyText] -> ScopeTree LazyText -> FieldSpec -> Maybe TextBuilder
genField currentScope namesInScope FieldSpec{..} = do
  fieldType' <- case fieldMod of
    Optional -> genType currentScope namesInScope fieldType
    Repeated -> genType currentScope namesInScope fieldType >>= \x -> pure ("[" <> x <> "]")
  pure $ fromString fieldName <> " :: " <> fieldType'

genType :: [LazyText] -> ScopeTree LazyText -> ProtoType -> Maybe TextBuilder
genType _ _ PDouble         = Just "Double"
genType _ _ PFloat          = Just "Float"
genType _ _ PInt32          = Just "Int32"
genType _ _ PInt64          = Just "Int64"
genType _ _ PUInt32         = Just "Word32"
genType _ _ PUInt64         = Just "Word64"
genType _ _ PSInt32         = Just "Int32"
genType _ _ PSInt64         = Just "Int64"
genType _ _ PFixed32        = Just "Word32"
genType _ _ PFixed64        = Just "Word64"
genType _ _ PSFixed32       = Just "Int32"
genType _ _ PSFixed64       = Just "Int64"
genType _ _ PBool           = Just "Bool"
genType _ _ PString         = Just "Text"
genType _ _ PBytes          = Just "ByteString"
genType scope names (NamedField s)
  | "." `List.isPrefixOf` s = resolveNameAbsolute (tail s)
  | otherwise               = resolveName s
  where
    possibleScopes :: [[LazyText]]
    possibleScopes = reverse $ List.inits scope

    resolveName :: String -> Maybe TextBuilder
    resolveName name = fmap normalizeType
                     . List.find (ScopeTree.contains names)
                     . fmap ( flip (++)
                            . Text.Lazy.splitOn "."
                            . fromString
                            $ name
                            )
                     $ possibleScopes

    resolveNameAbsolute :: String -> Maybe TextBuilder
    resolveNameAbsolute name
      | ScopeTree.contains names (Text.Lazy.splitOn "." $ fromString name)
          = Just $ normalizeType (Text.Lazy.splitOn "." $ fromString name)
      | otherwise = Nothing

    normalizeType :: [LazyText] -> TextBuilder
    normalizeType = intercalate "." . fmap (Text.Builder.fromLazyText . capitalizeText)

intercalate :: TextBuilder -> [TextBuilder] -> TextBuilder
intercalate _   []     = mempty
intercalate _   [x]    = x
intercalate mid (x:xs) = x <> mid <> intercalate mid xs

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs

capitalizeText :: LazyText -> LazyText
capitalizeText txt = case Text.Lazy.uncons txt of
    Just (h,t) -> Text.Lazy.cons (toUpper h) t
    Nothing    -> txt

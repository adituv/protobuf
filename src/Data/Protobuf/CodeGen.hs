{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Protobuf.CodeGen where

import           Data.Protobuf.ProtoSpec

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

genFiles :: LazyText -> ProtoSpec -> Either String [(FilePath, LazyText)]
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

genFilesAux :: LazyText -> ProtoSpec -> Either String (HashMap.HashMap FilePath LazyText)
genFilesAux scope spec@ProtoSpec{..} = do
    childFiles <- mapM (genFilesAux innerScope) innerSpecs
    thisFile <- genFile scope spec
    pure $ HashMap.insert fileName thisFile (HashMap.unions childFiles)
  where
    innerScope = Text.Builder.toLazyText
               $ addScope scope (fromString $ capitalize messageName)
    fileName = Text.Lazy.unpack $
               Text.Lazy.replace "." "/" innerScope <> ".hs"

genFile :: LazyText -> ProtoSpec -> Either String LazyText
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

genSpec :: LazyText -> ScopeTree LazyText -> ProtoSpec -> Either String TextBuilder
genSpec scope namesInScope ProtoSpec{..} = do
  fieldDefs <- genFields currentScope namesInScope fields
  pure $ "{-# LANGUAGE DeriveGeneric   #-}" <> endl
      <> "{-# LANGUAGE RecordWildCards #-}" <> endl
      <> "module " <> moduleName <> "(module Data.Protobuf, module " <> moduleName <> ") where" <> endl
      <> endl
      <> "import Data.Protobuf" <> endl
      <> "import Data.Protobuf.Encoding" <> endl
      <> "import qualified Data.IntMap as IntMap" <> endl
      <> "import GHC.Generics(Generic)" <> endl
      <> mconcat (genImport moduleName <$> innerSpecs)
      <> endl
      <> "data " <> datatypeName <> " = " <> datatypeName <> endl
      <> fieldDefs
      <> " deriving (Show, Generic)" <> endl <> endl
      <> genInstances scopedName fields
      <> endl
  where
    datatypeName = fromString $ capitalize messageName
    moduleName = addScope scope datatypeName
    scopedName = moduleName <> "." <> datatypeName
    currentScope = Text.Lazy.splitOn "." scope <> [Text.Lazy.pack messageName]

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
      "    $ IntMap.empty" <> endl <>
      foldMap genFieldToProto fields

    asRawValueInstance =
      "instance AsRawValue " <> scopedName <> " where" <> endl <>
      "  defaultValue =" <> endl <>
      "    " <> scopedName <> endl <>
      "      " <>
      intercalate (endl <> "      ") ("defaultValue" <$ fields) <> endl <>
      "  rawType = RTLengthEncoded" <> endl <>
      "  toRawValue msg = RLengthEncoded . runPut $ putRawMessage (toProto msg)" <> endl <>
      "  fromRawValue (RLengthEncoded raw) = case runGet getRawMessage raw of" <> endl <>
      "    Right rawMessage -> fromProto rawMessage" <> endl <>
      "    Left err         -> Failure err"

genFieldFromProto :: FieldSpec -> TextBuilder
genFieldFromProto FieldSpec{..} = "raw .: " <> fromString (show fieldTag)

genFieldToProto :: FieldSpec -> TextBuilder
genFieldToProto FieldSpec{..} = "      & IntMap.insert " <> fromString (show fieldTag) <> valueAsList <> endl
  where
      valueAsList = " [toRawValue " <> fromString fieldName <> "]"

genFields :: [LazyText] -> ScopeTree LazyText -> [FieldSpec] -> Either String TextBuilder
genFields currentScope namesInScope fields= do
  fieldSpecs <- mapM (genField currentScope namesInScope) fields
  pure $ "  { " <> intercalate (endl <> "  , ") fieldSpecs <> endl <> "  }"

genField :: [LazyText] -> ScopeTree LazyText -> FieldSpec -> Either String TextBuilder
genField currentScope namesInScope FieldSpec{..} = do
  fieldType' <- case fieldMod of
    Optional -> genType currentScope namesInScope fieldType
    Repeated -> genType currentScope namesInScope fieldType >>= \x -> pure ("[" <> x <> "]")
  pure $ fromString fieldName <> " :: " <> fieldType'

genType :: [LazyText] -> ScopeTree LazyText -> ProtoType -> Either String TextBuilder
genType _ _ PDouble         = Right "Double"
genType _ _ PFloat          = Right "Float"
genType _ _ PInt32          = Right "Int32"
genType _ _ PInt64          = Right "Int64"
genType _ _ PUInt32         = Right "Word32"
genType _ _ PUInt64         = Right "Word64"
genType _ _ PSInt32         = Right "Int32"
genType _ _ PSInt64         = Right "Int64"
genType _ _ PFixed32        = Right "Word32"
genType _ _ PFixed64        = Right "Word64"
genType _ _ PSFixed32       = Right "Int32"
genType _ _ PSFixed64       = Right "Int64"
genType _ _ PBool           = Right "Bool"
genType _ _ PString         = Right "Text"
genType _ _ PBytes          = Right "ByteString"
genType scope names (NamedField s)
  | "." `List.isPrefixOf` s = resolveNameAbsolute (tail s)
  | otherwise               = resolveName s
  where
    possibleScopes :: [[LazyText]]
    possibleScopes = reverse $ List.inits scope

    resolveName :: String -> Either String TextBuilder
    resolveName name = maybe (Left $ "Could not resolve name " <> name)
                             (Right . normalizeType)
                     . List.find (ScopeTree.contains names)
                     . fmap ( flip (++)
                            . Text.Lazy.splitOn "."
                            . fromString
                            $ name
                            )
                     $ possibleScopes

    resolveNameAbsolute :: String -> Either String TextBuilder
    resolveNameAbsolute name
      | ScopeTree.contains names (Text.Lazy.splitOn "." $ fromString name)
          = Right $ normalizeType (Text.Lazy.splitOn "." $ fromString name)
      | otherwise = Left $ "Could not resolve name " <> name

    normalizeType :: [LazyText] -> TextBuilder
    normalizeType = intercalate "." . (\xs -> xs <> [last xs]) . fmap (Text.Builder.fromLazyText . capitalizeText)

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

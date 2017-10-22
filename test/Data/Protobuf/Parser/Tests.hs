{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Data.Protobuf.Parser.Tests(parserTests) where

import Data.Protobuf.Parser.Internal
import Data.Protobuf.ProtoSpec

import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Test.QuickCheck.Instances

import Data.Semigroup((<>))
import qualified Data.Text as T
import Text.Megaparsec

parserTests :: Spec
parserTests = describe "Protobuf parser" $ do
  identifierTests
  protoTypeTests

-- * Test utilities

anyIdentifier :: Gen String
anyIdentifier = (:) <$> anyAlpha <*> listOf anyIdChar

anyAlpha :: Gen Char
anyAlpha = oneof [upperAlpha, lowerAlpha]

anyIdChar :: Gen Char
anyIdChar = frequency [(52, anyAlpha), (10, numeric), (1, pure '_')]

badIdentifier :: Gen String
badIdentifier = do
    -- guarantee identifier of length at least 2
    ident <- anyIdentifier
    bad <- elements ['!'..'/']
    pos <- choose (0, length ident - 1)
    let (start, end) = splitAt pos ident
    pure (start ++ bad:end)

-- String representations of scalar type names
typeStrings :: [(ProtoType, String)]
typeStrings = [ (PDouble, "double")
              , (PFloat, "float")
              , (PInt32, "int32")
              , (PInt64, "int64")
              , (PUInt32, "uint32")
              , (PUInt64, "uint64")
              , (PSInt32, "sint32")
              , (PSInt64, "sint64")
              , (PFixed32, "fixed32")
              , (PFixed64, "fixed64")
              , (PBool, "bool")
              , (PString, "string")
              , (PBytes, "bytes")
              ]

scalarTypeString :: Gen (ProtoType, String)
scalarTypeString = elements typeStrings

anyTypeString :: Gen (ProtoType, String)
anyTypeString = oneof [scalarTypeString, nonTSIdent]
  where
    nonTSIdent = do
      ident' <- anyIdentifier `suchThat`
                  (\e -> e `notElem` fmap snd typeStrings)
      pure (NamedField ident', ident')

-- * Tests

identifierTests :: Spec
identifierTests = describe "identifier" $ do
  it "accepts a single-letter identifier" $
    runParser identifier "" "a" `shouldParse` "a"
  it "accepts a longer identifier" $
    runParser identifier "" "foo345bar6" `shouldParse` "foo345bar6"
  it "accepts randomly-generated identifiers" $ property $
    forAll anyIdentifier $
      \t -> runParser identifier "" (T.pack t) `shouldParse` t
  it "fails to parse integers" $ property $ \(Positive (t :: Int)) -> 
    runParser identifier "" (T.pack $ show t) `shouldFailWith`
      err posI ((utok . head . show $ t) <> elabel "identifier")
  it "fails to parse bad identifiers" $ property $
    forAll badIdentifier $
      \s -> runParser (identifier <* eof) "" `shouldFailOn` T.pack s

protoTypeTests :: Spec
protoTypeTests = describe "protoType" $ do
  it "accepts double" $
    runParser protoType "" "double" `shouldParse` PDouble
  it "accepts string" $
    runParser protoType "" "string" `shouldParse` PString
  it "accepts an identifier" $ 
    runParser protoType "" "test3" `shouldParse` (NamedField "test3")
  it "accepts any scalar type" $ property $ forAll scalarTypeString $
    \(expected, str) -> runParser protoType "" (T.pack str)
      `shouldParse` expected
  it "accepts any valid type string" $ property $ forAll anyTypeString $
    \(expected, str) -> runParser protoType "" (T.pack str)
      `shouldParse` expected
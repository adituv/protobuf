{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Protobuf.Parser.Tests
  ( parserTests
  ) where

import           Data.Protobuf.Parser.Internal
import           Data.Protobuf.ProtoSpec

import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Test.QuickCheck
import           Test.QuickCheck.Instances

import           Control.Applicative           (liftA2, liftA3)
import           Control.Monad                 (replicateM)
import           Data.Bifunctor                (first)
import           Data.Semigroup                ((<>))
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Text.Megaparsec

parserTests :: Spec
parserTests =
  describe "Protobuf parser" $ do
    identifierTests
    protoTypeTests
    reservationTests
    fieldModifierTests
    fieldSpecTests
    specBodyEntryTests
    protoSpecBodyTests
    protoSpecTests
    proto3FileTests

-- * Test utilities
anyIdentifier :: Gen String
anyIdentifier = (:) <$> anyAlpha <*> listOf anyIdChar

anyAlpha :: Gen Char
anyAlpha = oneof [upperAlpha, lowerAlpha]

anyIdChar :: Gen Char
anyIdChar = frequency [(52, anyAlpha), (10, numeric), (1, pure '_')]

badIdentifier :: Gen String
badIdentifier = do
  ident <- anyIdentifier
  bad <- elements ['!' .. '/']
  pos <- choose (0, length ident - 1)
  let (start, end) = splitAt pos ident
  pure (start ++ bad : end)

-- String representations of scalar type names
typeStrings :: [(ProtoType, String)]
typeStrings =
  [ (PDouble, "double")
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
      ident' <-
        anyIdentifier `suchThat` (\e -> e `notElem` fmap snd typeStrings
                              && e /= "reserved" && e /= "message")
      pure (NamedField ident', ident')

repeatedFieldSpec :: Gen T.Text
repeatedFieldSpec = do
  restOfSpec <- optionalFieldSpec
  pure $ "repeated " <> restOfSpec

optionalFieldSpec :: Gen T.Text
optionalFieldSpec = do
  (_, type') <- anyTypeString
  name <- anyIdentifier
  tag <- show <$> (positive :: Gen Int)
  pure . T.pack $ type' <> " " <> name <> " = " <> tag <> ";"

anyFieldSpec :: Gen T.Text
anyFieldSpec = oneof [repeatedFieldSpec, optionalFieldSpec]

anyResTagEntry :: Gen T.Text
anyResTagEntry = oneof $ fmap (T.pack) <$> [singleTag, tagRange, tagToMax]
  where
    singleTag = show <$> (positive :: Gen Int)
    tagRange = do
      t1 <- singleTag
      t2 <- singleTag
      pure (t1 <> " to " <> t2)
    tagToMax = (<> " to max") <$> singleTag

multipleTagEntries :: Int -> Gen (Int, T.Text)
multipleTagEntries max' = do
  n <- choose (1, max')
  entries <- replicateM n anyResTagEntry
  pure $ (n, T.intercalate ", " entries)

anyResNameEntry :: Gen T.Text
anyResNameEntry = do
  n <- anyIdentifier
  pure . T.pack $ "\"" <> n <> "\""

multipleNameEntries :: Int -> Gen (Int, T.Text)
multipleNameEntries max' = do
  n <- choose (1, max')
  entries <- replicateM n anyResNameEntry
  pure $ (n, T.intercalate ", " entries)

-- * Tests
identifierTests :: Spec
identifierTests =
  describe "identifier" $ do
    it "parses a single-letter identifier" $
      runParser identifier "" "a" `shouldParse` "a"

    it "parses a longer identifier" $
      runParser identifier "" "foo345bar6" `shouldParse` "foo345bar6"

    it "parses randomly-generated identifiers" $
      property $
      forAll anyIdentifier $ \t ->
        runParser identifier "" (T.pack t) `shouldParse` t

    it "fails to parse integers" $
      property $ \(Positive (t :: Int)) ->
        runParser identifier "" (T.pack $ show t) `shouldFailWith`
        err posI ((utok . head . show $ t) <> elabel "identifier")

    it "fails to parse bad identifiers" $
      property $
      forAll badIdentifier $ \s ->
        runParser (identifier <* eof) "" `shouldFailOn` T.pack s

protoTypeTests :: Spec
protoTypeTests =
  describe "protoType" $ do
    it "parses double" $ runParser protoType "" "double" `shouldParse` PDouble

    it "parses string" $ runParser protoType "" "string" `shouldParse` PString

    it "parses an identifier" $
      runParser protoType "" "test3" `shouldParse` NamedField "test3"

    it "parses any scalar type" $
      property $
      forAll scalarTypeString $ \(expected, str) ->
        runParser protoType "" (T.pack str) `shouldParse` expected

    it "parses any valid type string" $
      property $
      forAll anyTypeString $ \(expected, str) ->
        runParser protoType "" (T.pack str) `shouldParse` expected

    it "fails to parse bad identifiers" $ property $
      forAll badIdentifier $ \s ->
        runParser (protoType <* eof) "" `shouldFailOn` (T.pack s)

    it "fails to parse \"reserved\"" $
      runParser (protoType <* eof) "" `shouldFailOn` "reserved"

reservationTests :: Spec
reservationTests =
  describe "reservation" $ do
    it "parses a single name reservation" $
      runParser reservation "" "reserved \"blah\";"
      `shouldParse` [RName "blah"]

    it "parses a single randomly-generated name reservation" $
      property $
      forAll anyIdentifier $ \t ->
        runParser reservation "" ("reserved \"" <> T.pack t <> "\";")
        `shouldParse` [RName t]

    it "parses a single tag reservation" $
      runParser reservation "" "reserved 3;" `shouldParse` [RTag 3]

    it "parses a single randomly-generated tag reservation" $
      property $ \(Positive t) ->
        runParser reservation "" ("reserved " <> T.pack (show t) <> ";")
        `shouldParse` [RTag t]

    it "parses a tag range reservation" $
      runParser reservation "" "reserved 3 to 10;"
      `shouldParse` [RTagRange 3 10]

    it "parses a randomly-generated tag range reservation" $
      property $ \(Positive x) (Positive y) ->
        runParser
          reservation
          ""
          ("reserved " <> T.pack (show x) <> " to " <> T.pack (show y) <> ";")
        `shouldParse` [RTagRange x y]

    it "parses a tag range to max reservation" $
      runParser reservation "" "reserved 100 to max;" `shouldParse`
      [RTagToMax 100]

    it "parses a random tag range to max reservation" $
      property $ \(Positive t) ->
        runParser
          reservation
          ""
          ("reserved " <> T.pack (show t) <> " to max;")
          `shouldParse` [RTagToMax t]

    it "parses multiple tag reservations in one line" $
      property $ forAll (multipleTagEntries 40) $ \(n,t) ->
        runParser reservation "" ("reserved " <> t <> ";")
          `parseSatisfies` \res -> length res == n

    it "parses multiple name reservations in one line" $
      property $ forAll (multipleNameEntries 40) $ \(n,t) ->
        runParser reservation "" ("reserved " <> t <> ";")
          `parseSatisfies` \res -> length res == n


    it "fails to parse a bare identifier" $
      let input = "reserved test" :: T.Text
      in  runParser reservation "" input `shouldFailWith`
          err
            (posN (9 :: Int) input)
            (utok 't' <> elabel "decimal integer" <>
             elabel "identifier string")

    it "fails to parse with a missing semicolon" $
      let input = "reserved 50" :: T.Text
      in  runParser reservation "" input `shouldFailWith`
          err
            (posN (12 :: Int) input)
            (ueof <> etoks "to" <> etok ',' <> etok ';' <>
             elabel "the rest of decimal integer")

    it "fails and consumes no input when attempting to parse a fieldspec" $
      property $ forAll anyFieldSpec $ \spec ->
        runParser' reservation (initialState spec) `failsLeaving` spec

    it "fails and consumes no input when attempting to parse a message" $
      property $ forAll (liftA2 (,) anyIdentifier anyIdentifier) $
        \(t1,t2) -> do
          let spec' = "message " <> (T.pack t1) <> " {\n\tuint32 "
                                 <> (T.pack t2) <> " = 1;\n}\n"
          runParser' reservation (initialState spec') `failsLeaving` spec'


fieldModifierTests :: Spec
fieldModifierTests =
  describe "fieldModifier" $ do
    it "parses \"repeated\"" $
      runParser fieldModifier "" "repeated" `shouldParse` Repeated

    it "parses Optional when not \"repeated\"" $ property $
      forAll anyIdentifier $ \t ->
        t /= "repeated" ==> runParser fieldModifier "" (T.pack t)
          `shouldParse` Optional

    it "consumes no input for any identifier not \"repeated\"" $ property $
      forAll anyIdentifier $ \t ->
        t /= "repeated" ==> runParser' fieldModifier (initialState $ T.pack t)
          `succeedsLeaving` (T.pack t)

fieldSpecTests :: Spec
fieldSpecTests =
  describe "fieldSpec" $ do
    it "parses any fieldspec with repeated as having Repeated" $ property $
      forAll repeatedFieldSpec $ \t -> runParser fieldSpec "" t
        `parseSatisfies` \FieldSpec{..} -> fieldMod == Repeated

    it "parses any fieldspec without repeated as Optional" $ property $
      forAll optionalFieldSpec $ \t -> runParser fieldSpec "" t
        `parseSatisfies` \FieldSpec{..} -> fieldMod == Optional

    it "parses the type of a fieldspec correctly" $ property $
      forAll (liftA2 (,) anyFieldSpec anyTypeString) $ \(spec, (pt, tt)) -> do
        spec' <- case (T.words spec) of
          [_type, ident', eq, tag] ->
            pure $ T.unwords [T.pack tt, ident', eq, tag]
          [res, _type, ident', eq, tag] ->
            pure $ T.unwords [res, T.pack tt, ident', eq, tag]
          _ -> fail "Unexpected number of words in fieldspec"

        runParser fieldSpec "" spec' `parseSatisfies` \FieldSpec{..} ->
          fieldType == pt


    it "parses the name of a fieldspec correctly" $ property $
      forAll (liftA2 (,) anyFieldSpec anyIdentifier) $ \(spec, name') -> do
        spec' <- case (T.words spec) of
          [type', _ident, eq, tag] ->
            pure $ T.unwords [type', T.pack name', eq, tag]
          [res, type', _ident, eq, tag] ->
            pure $ T.unwords [res, type', T.pack name', eq, tag]
          _ -> fail "Unexpected number of words in fieldspec"

        runParser fieldSpec "" spec' `parseSatisfies` \FieldSpec{..} ->
          fieldName == name'


    it "parses the tag of a fieldspec correctly" $ property $
      forAll anyFieldSpec $ \spec (Positive tag) -> do
        let tag' = T.pack $ show (tag :: Int)
        spec' <- case (T.words spec) of
          [type', ident', eq, _tag] ->
            pure $ T.unwords [type', ident', eq, tag' <> ";"]
          [res, type', ident', eq, _tag] ->
            pure $ T.unwords [res, type', ident', eq, tag' <> ";"]
          _ -> fail "Unexpected number of words in fieldspec"

        runParser fieldSpec "" spec' `parseSatisfies` \FieldSpec{..} ->
          fieldTag == tag


    it "fails and consumes no input when attempting to parse a reservation" $
      property $ forAll anyResTagEntry $ \resTag ->
        let
          reserved' :: T.Text
          reserved' = "reserved " <> resTag <> ";"
        in
          runParser' fieldSpec (initialState $ reserved')
            `failsLeaving` reserved'

    it "fails and consumes no input when attempting to parse a message" $
      property $ forAll (liftA2 (,) anyIdentifier anyIdentifier) $
        \(t1,t2) -> do
          let spec' = "message " <> (T.pack t1) <> " {\n\tuint32 "
                                 <> (T.pack t2) <> " = 1;\n}\n"
          runParser' fieldSpec (initialState spec') `failsLeaving` spec'

specBodyEntryTests :: Spec
specBodyEntryTests =
  describe "specBodyEntry" $ do
    it "parses a FieldEntry" $ property $ forAll anyFieldSpec $ \spec ->
      runParser specBodyEntry "" spec `parseSatisfies` \e -> case e of
        FieldEntry _ -> True
        _            -> False

    it "parses a ReservedEntry" $ property $ forAll anyResTagEntry $ \tags ->
      runParser specBodyEntry "" ("reserved " <> tags <> ";") `parseSatisfies`
        \e -> case e of
          ReservedEntry _ -> True
          _               -> False

    it "parses a MessageEntry" $ property $
      forAll (liftA3 (,,) anyIdentifier anyTypeString anyIdentifier) $
        \(mname,(_type,typeS),idname) (Positive tag) -> do
          let spec' = "message " <> (T.pack mname) <> " {\n\t" <> (T.pack typeS)
                          <> " " <> (T.pack idname) <> " = "
                          <> (T.pack $ show (tag :: Int)) <> ";\n}\n"
          runParser specBodyEntry "" spec' `parseSatisfies`
            \e -> case e of
              MessageEntry _ -> True
              _              -> False


-- At this point the nesting in the parsing is too complicated to deal with
-- directly, so this uses a roll-your-own version of golden testing.  I don't
-- want to port all these tests to a different test framework (tasty) unless
-- I have to.
protoSpecBodyTests :: Spec
protoSpecBodyTests =
  describe "protoSpecBody" $
    mapM_ (goldenTest protoSpecBody "protoSpecBody") [1..3]

protoSpecTests :: Spec
protoSpecTests =
  describe "protoSpec" $
    mapM_ (goldenTest protoSpec "protoSpec") [1..3]

proto3FileTests :: Spec
proto3FileTests =
  describe "proto3File" $
    mapM_ (goldenTest proto3File "proto3File") [1..6]

goldenTest :: Show a => Parsec Dec T.Text a -> String -> Int -> Spec
goldenTest parser dir n =
  before
    (liftA2 (,) (TIO.readFile $ "test-data/" <> dir <> "/test"
                                <> show n <> "-input.txt")
                (TIO.readFile $ "test-data/" <> dir <> "/test"
                                <> show n <> "-output.txt"))
    (it ("Correctly parses the sample input #" <> show n) $ \(t1, t2) ->
        T.pack (show . first parseErrorPretty $ runParser parser "" t1) `shouldBe` t2)

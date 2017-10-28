{-# LANGUAGE RecordWildCards #-}
module Data.Protobuf.Parser.Internal
  ( -- * Public interface
    identifier
  , protoSpec
  , protoType
    -- * Implementation details
  , SpecBodyEntry(..)
  , protoSpecBody
  , specBodyEntry
  , fieldSpec
  , fieldModifier
  , reservation
  ) where

import           Data.Protobuf.ProtoSpec

import           Control.Monad           (guard, mzero, void)
import           Data.Char
import           Data.List               (foldl')
import           Data.Text               (Text)
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer   as L

-- | Parser that skips all whitespace and line comments
spaceConsumer :: Parsec Dec Text ()
spaceConsumer = L.space (void spaceChar) -- space between lexemes
                        (L.skipLineComment "//") -- line comments
                        (L.skipBlockComment "/*" "*/") -- block comments

-- | Utility to wrap a parser for a lexeme so that it also consumes
--   following space characters.  Useful when no explicit lexer phase
--   is used.
lexeme :: Parsec Dec Text a -> Parsec Dec Text a
lexeme = L.lexeme spaceConsumer

-- | Parses a lexeme of a literal symbol.
symbol :: String -> Parsec Dec Text String
symbol = try . L.symbol spaceConsumer

-- | Reserved words
rwords :: [String]
rwords = ["message", "reserved"]

-- | Parses an ascii letter in either uppercase or lowercase.
alphaChar :: Parsec Dec Text Char
alphaChar = satisfy ((||) <$> isAsciiUpper <*> isAsciiLower) <?> "letter"

decimal :: Parsec Dec Text Int
decimal = lexeme $ fromInteger <$> L.decimal

-- | Parses a valid protobuf identifier.  **Not a lexeme.**
ident :: Parsec Dec Text String
ident = (<?> "identifier") . try $ do
  h <- alphaChar
  t <- many (alphaChar <|> digitChar <|> char '_')
  let id' = h:t
  guard $ id' `notElem` rwords
  pure id'

-- | Parses a valid protobuf identifier as a lexeme.
identifier :: Parsec Dec Text String
identifier = lexeme ident

identString :: Parsec Dec Text String
identString = between (char '"') (char '"') ident <?> "identifier string"

protoSpec :: Parsec Dec Text ProtoSpec
protoSpec = (do
    void $ symbol "message"
    messageName <- lexeme identifier
    between (symbol "{") (symbol "}") $ do
      (innerSpecs, fields, reserved) <- protoSpecBody
      pure ProtoSpec{..}) <?> "message specification"

protoSpecBody :: Parsec Dec Text ([ProtoSpec], [FieldSpec], [Reservation])
protoSpecBody = recombine <$> many specBodyEntry
  where
    recombine :: [SpecBodyEntry] -> ([ProtoSpec], [FieldSpec], [Reservation])
    recombine = reverseEntries . foldl' recombine' ([],[],[])

    recombine' (ms,fs,rs) (MessageEntry m)    = (m:ms, fs, rs)
    recombine' (ms,fs,rs) (FieldEntry f)      = (ms, f:fs, rs)
    recombine' (ms,fs,rs) (ReservedEntry rs') = (ms, fs, rs ++ rs')

    reverseEntries (ms, fs, rs) = (reverse ms, reverse fs, rs)

data SpecBodyEntry = MessageEntry ProtoSpec
                   | FieldEntry FieldSpec
                   | ReservedEntry [Reservation]
                   deriving Show

specBodyEntry :: Parsec Dec Text SpecBodyEntry
specBodyEntry =  MessageEntry <$> protoSpec
             <|> FieldEntry <$> fieldSpec
             <|> ReservedEntry <$> reservation

fieldSpec :: Parsec Dec Text FieldSpec
fieldSpec = (do
    fieldMod <- fieldModifier
    fieldType <- protoType
    fieldName <- identifier
    void $ symbol "="
    fieldTag <- decimal
    void $ symbol ";"
    pure FieldSpec{..}) <?> "field specification"

fieldModifier :: Parsec Dec Text FieldModifier
fieldModifier =  Repeated <$ symbol "repeated"
             <|> pure Optional
             <?> "field modifier"

protoType :: Parsec Dec Text ProtoType
protoType =  PDouble <$ symbol "double"
         <|> PFloat <$ symbol "float"
         <|> PInt32 <$ symbol "int32"
         <|> PInt64 <$ symbol "int64"
         <|> PUInt32 <$ symbol "uint32"
         <|> PUInt64 <$ symbol "uint64"
         <|> PSInt32 <$ symbol "sint32"
         <|> PSInt64 <$ symbol "sint64"
         <|> PFixed32 <$ symbol "fixed32"
         <|> PFixed64 <$ symbol "fixed64"
         <|> PBool <$ symbol "bool"
         <|> PString <$ symbol "string"
         <|> PBytes <$ symbol "bytes"
         <|> NamedField <$> identifier
         <?> "protobuf type"

reservation :: Parsec Dec Text [Reservation]
reservation = (do
    void $ symbol "reserved"
    rs <- rtags `sepBy1` symbol "," <|> rnames `sepBy1` symbol ","
    void $ symbol ";"
    pure rs) <?> "reserved statement"
  where
    rnames :: Parsec Dec Text Reservation
    rnames =  RName <$> identString

    rtags :: Parsec Dec Text Reservation
    rtags = do
      n <- decimal
      range <- optional $ symbol "to" *> (RTagToMax <$ symbol "max"
                                     <|> flip RTagRange <$> decimal)
      case range of
        Nothing -> pure $ RTag n
        Just f  -> pure $ f n

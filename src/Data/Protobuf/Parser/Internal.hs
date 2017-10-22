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

import Data.Protobuf.ProtoSpec

import Control.Monad(mzero, void)
import Data.Char
import Data.List(foldl')
import Data.Text(Text)
import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L

-- | Parser that skips all whitespace and line comments
spaceConsumer :: Parsec () Text ()
spaceConsumer = L.space (void spaceChar) (L.skipLineComment "//") mzero

-- | Utility to wrap a parser for a lexeme so that it also consumes
--   following space characters.  Useful when no explicit lexer phase
--   is used.
lexeme :: Parsec () Text a -> Parsec () Text a
lexeme = L.lexeme spaceConsumer

-- | Parses a lexeme of a literal symbol.
symbol :: String -> Parsec () Text String
symbol = try . L.symbol spaceConsumer

-- | Parses an ascii letter in either uppercase or lowercase.
alphaChar :: Parsec () Text Char
alphaChar = satisfy ((||) <$> isAsciiUpper <*> isAsciiLower)

decimal :: Parsec () Text Int
decimal = lexeme $ read <$> many digitChar

-- | Parses a valid protobuf identifier.  **Not a lexeme.**
ident :: Parsec () Text String
ident = (:) <$> letterChar <*> many (alphaChar <|> digitChar <|> char '_')

-- | Parses a valid protobuf identifier as a lexeme.
identifier :: Parsec () Text String
identifier = lexeme ident

identString :: Parsec () Text String
identString = between (char '"') (char '"') identifier

protoSpec :: Parsec () Text ProtoSpec
protoSpec = do
    void $ symbol "message"
    messageName <- lexeme identifier
    (innerSpecs, fields, reserved) <- protoSpecBody
    pure ProtoSpec{..}

protoSpecBody :: Parsec () Text ([ProtoSpec], [FieldSpec], [Reservation])
protoSpecBody = recombine <$> many specBodyEntry
  where
    recombine :: [SpecBodyEntry] -> ([ProtoSpec], [FieldSpec], [Reservation])
    recombine = foldl' recombine' ([],[],[])

    recombine' (ms,fs,rs) (MessageEntry m) = (m:ms, fs, rs)
    recombine' (ms,fs,rs) (FieldEntry f) = (ms, f:fs, rs)
    recombine' (ms,fs,rs) (ReservedEntry rs') = (ms, fs, rs ++ rs')

data SpecBodyEntry = MessageEntry ProtoSpec
                   | FieldEntry FieldSpec
                   | ReservedEntry [Reservation]
                   deriving Show

specBodyEntry :: Parsec () Text SpecBodyEntry
specBodyEntry =  MessageEntry <$> protoSpec
             <|> FieldEntry <$> fieldSpec
             <|> ReservedEntry <$> reservation

fieldSpec :: Parsec () Text FieldSpec
fieldSpec = do
    fieldMod <- fieldModifier
    fieldType <- protoType
    fieldName <- identifier
    void $ symbol "="
    fieldTag <- decimal
    void $ symbol ";"
    pure FieldSpec{..}

fieldModifier :: Parsec () Text FieldModifier
fieldModifier =  Repeated <$ symbol "repeated"
             <|> pure Optional

protoType :: Parsec () Text ProtoType
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

reservation :: Parsec () Text [Reservation]
reservation = do
    void $ symbol "reserved"
    res `sepBy` symbol ","
  where
    res :: Parsec () Text Reservation
    res =  RName <$> identString
       <|> rtags

    -- TODO: rewrite for clarity?
    rtags :: Parsec () Text Reservation
    rtags = flip ($) <$> decimal <*> (symbol "to" *>
        ((RTagToMax <$ symbol "max")
            <|> (flip RTagRange <$> decimal)
            <|> pure RTag))
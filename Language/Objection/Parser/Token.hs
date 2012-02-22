{-# LANGUAGE FlexibleContexts #-}
module Language.Objection.Parser.Token
    (
    tokenize,
    Token(..),
    )
where

import Control.Applicative ((<$>), (*>), (<*))
import Control.Monad.Identity (Identity)
import Data.Int
import Text.Parsec

data Token = TAdd
             | TChar
             | TClass
             | TComma
             | TDivide
             | TDot
             | TDoubleEquals
             | TEquals
             | TG
             | TGE
             | TIdentifier String
             | TInt
             | TIntLiteral Int32
             | TL
             | TLE
             | TLeftBrace
             | TLeftBracket
             | TLeftParen
             | TLong
             | TMultiply
             | TPrivate
             | TProtected
             | TPublic
             | TRightBrace
             | TRightBracket
             | TRightParen
             | TSemicolon
             | TSubtract
             | TVoid
             deriving (Show, Read)

tokenize :: Stream s Identity Char
            => SourceName
            -> s
            -> Either ParseError [(SourcePos, Token)]
tokenize = parse tokensP

stringToken :: (Stream s m Char, Monad m) 
               => String
               -> Token
               -> ParsecT s u m Token
stringToken s t = try (string s) *> return t

charToken :: (Stream s m Char, Monad m)
             => Char
             -> Token
             -> ParsecT s u m Token
charToken c t = char c *> return t

integerLiteralP :: (Stream s m Char, Monad m) => ParsecT s u m Int32
integerLiteralP = read <$> try (many1 digit)

identifierP :: (Stream s m Char, Monad m) => ParsecT s u m String
identifierP = try (many1 letter)

tokensP :: (Stream s m Char, Monad m) => ParsecT s u m [(SourcePos, Token)]
tokensP = many $ spaces *> tokenP <* spaces

tokenP :: (Stream s m Char, Monad m) => ParsecT s u m (SourcePos, Token)
tokenP = do pos <- getPosition
            tok <- charToken '+' TAdd
                   <|> stringToken "char" TChar
                   <|> stringToken "class" TClass
                   <|> charToken ',' TComma
                   <|> charToken '/' TDivide
                   <|> charToken '.' TDot
                   <|> stringToken "==" TDoubleEquals
                   <|> charToken '=' TEquals
                   <|> stringToken ">=" TGE
                   <|> charToken '>' TG
                   <|> stringToken "int" TInt
                   <|> stringToken "<=" TLE
                   <|> charToken '<' TL
                   <|> charToken '{' TLeftBrace
                   <|> charToken '[' TLeftBracket
                   <|> charToken '(' TLeftParen
                   <|> stringToken "long" TLong
                   <|> charToken '*' TMultiply
                   <|> stringToken "private" TPrivate
                   <|> stringToken "protected" TProtected
                   <|> stringToken "public" TPublic
                   <|> charToken '}' TRightBrace
                   <|> charToken ']' TRightBracket
                   <|> charToken ')' TRightParen
                   <|> charToken ';' TSemicolon
                   <|> charToken '-' TSubtract
                   <|> stringToken "void" TVoid
                   <|> (TIntLiteral <$> integerLiteralP)
                   <|> (TIdentifier <$> identifierP)
                   <?> "Token"
            return (pos, tok)

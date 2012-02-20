{-# LANGUAGE FlexibleContexts #-}
module Language.Objection.Parser.TokenStream
where 

import Control.Monad.Identity (Identity)
import Data.Int
import Text.Parsec hiding (token)
import qualified Text.Parsec as P

import Language.Objection.Parser.Token

basicToken :: (Stream s Identity (SourcePos, Token))
               => (Token -> Maybe a)
               -> Parsec s u a
basicToken f = P.token showToken posFromToken testToken
    where showToken (_, t) = show t
          posFromToken (p, _) = p
          testToken (_, t) = f t

-- | Gets any token
token :: (Stream s Identity (SourcePos, Token)) => Parsec s u Token
token = basicToken Just

addToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
addToken = basicToken f
    where f TAdd = Just ()
          f _ = Nothing

charToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
charToken = basicToken f
    where f TChar = Just ()
          f _ = Nothing

classToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
classToken = basicToken f
    where f TClass = Just ()
          f _ = Nothing

commaToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
commaToken = basicToken f
    where f TComma = Just ()
          f _ = Nothing

divideToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
divideToken = basicToken f
    where f TDivide = Just ()
          f _ = Nothing

dotToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
dotToken = basicToken f
    where f TDot = Just ()
          f _ = Nothing

equalsToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
equalsToken = basicToken f
    where f TEquals = Just ()
          f _ = Nothing

identifierToken :: (Stream s Identity (SourcePos, Token))
                   => Parsec s u String
identifierToken = basicToken f
    where f (TIdentifier s) = Just s
          f _ = Nothing

intToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
intToken = basicToken f
    where f TInt = Just ()
          f _ = Nothing

intLiteralToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u Int32
intLiteralToken = basicToken f
    where f (TIntLiteral i) = Just i
          f _ = Nothing

leftBraceToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
leftBraceToken = basicToken f
    where f (TLeftBrace) = Just ()
          f _ = Nothing

leftBracketToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
leftBracketToken = basicToken f
    where f (TLeftBracket) = Just ()
          f _ = Nothing

leftParenToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
leftParenToken = basicToken f
    where f (TLeftParen) = Just ()
          f _ = Nothing

longToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
longToken = basicToken f
    where f TLong = Just ()
          f _ = Nothing

multiplyToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
multiplyToken = basicToken f
    where f TMultiply = Just ()
          f _ = Nothing

privateToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
privateToken = basicToken f
    where f TPrivate = Just ()
          f _ = Nothing

protectedToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
protectedToken = basicToken f
    where f TProtected = Just ()
          f _ = Nothing
          
publicToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
publicToken = basicToken f
    where f TPublic = Just ()
          f _ = Nothing
          
rightBraceToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
rightBraceToken = basicToken f
    where f TRightBrace = Just ()
          f _ = Nothing

rightBracketToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
rightBracketToken = basicToken f
    where f TRightBracket = Just ()
          f _ = Nothing

rightParenToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
rightParenToken = basicToken f
    where f TRightParen = Just ()
          f _ = Nothing

semicolonToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
semicolonToken = basicToken f
    where f TSemicolon = Just ()
          f _ = Nothing

subtractToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
subtractToken = basicToken f
    where f TSubtract = Just ()
          f _ = Nothing
voidToken :: (Stream s Identity (SourcePos, Token)) => Parsec s u ()
voidToken = basicToken f
    where f TVoid = Just ()
          f _ = Nothing

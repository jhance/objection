{-# LANGUAGE FlexibleContexts #-}
module Language.Objection.Parser.List
    (list)
where

import Control.Monad.Identity (Identity)
import Text.Parsec
import Text.Parsec.Expr

import Language.Objection.Parser.Token
import Language.Objection.Parser.TokenStream

list :: Stream s Identity (SourcePos, Token)
        => Parsec s u a
        -> Parsec s u [a]
list p = buildExpressionParser table p'
    where p' = do x <- p
                  return [x]

table :: Stream s Identity (SourcePos, Token)
           => [[Operator s u Identity [a]]]
table = [[binary commaToken (++)]]

binary t f = Infix (t >> return f) AssocLeft

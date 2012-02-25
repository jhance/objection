{-# LANGUAGE FlexibleContexts #-}
module Language.Objection.Parser
    (
    parseModule
    )
where

import Control.Applicative ((<$>), (<*>), (*>), (<*))
import Control.Monad.Identity (Identity)
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token (reservedOp)

import Language.Objection.SyntaxTree
import Language.Objection.Parser.List
import Language.Objection.Parser.Token
import Language.Objection.Parser.TokenStream

data ClassItem = DField Field | DConstructor Constructor | DMethod Method

parseModule :: Stream s Identity Char
               => SourceName
               -> s
               -> Either ParseError Module
parseModule source stream =
    let tokens = tokenize source stream
    in case tokens of
        Left e -> Left e
        Right tokens' -> parse moduleP (source ++ ".tokens") tokens'
                            

-- | Called mod since module is reserved word
moduleP :: Stream s Identity (SourcePos, Token) => Parsec s u Module
moduleP = do classes <- many classP
             let classesKV = map f classes
                 f c@(Class i _ _ _ _) = (i, c)
             return . Module . M.fromList $ classesKV

privacyLevel :: Stream s Identity (SourcePos, Token) => Parsec s u PrivacyLevel
privacyLevel = (privateToken *> return Private)
                <|> (protectedToken *> return Protected)
                <|> (publicToken *> return Public)
                <?> "Privacy level"

classP :: Stream s Identity (SourcePos, Token) => Parsec s u Class
classP = do priv <- privacyLevel
            classToken
            id <- identifierToken
            leftBraceToken
            (cs, fs, ms) <- classDefinition
            rightBraceToken
            return $ Class id priv cs fs ms
         <?> "Class"

classDefinition :: Stream s Identity (SourcePos, Token)
                   => Parsec s u ([Constructor], M.Map Identifier Field, M.Map Identifier Method)
classDefinition = do items <- many classItem
                     return $ collapseItems items
                  <?> "Class definition"

classItem :: Stream s Identity (SourcePos, Token) => Parsec s u ClassItem
classItem = ((DField <$> try field) <|> (DMethod <$> method))
            <?> "Method, Field, or Constructor"
            
collapseItems :: [ClassItem]
                 -> ([Constructor], M.Map Identifier Field, M.Map Identifier Method)
collapseItems items = g ([], M.empty, M.empty) items
    where g acc [] = acc
          g (cs, fs, ms) (x:xs) =
            case x of
                DConstructor c -> g (c:cs, fs, ms) xs
                DField f@(Field i _ _) -> g (cs, M.insert i f fs, ms) xs
                DMethod m@(Method i _ _ _ _) -> g (cs, fs, M.insert i m ms) xs

typeP :: Stream s Identity (SourcePos, Token) => Parsec s u Type
typeP = (intToken *> return (PrimitiveType PrimitiveInt))
        <|> (longToken *> return (PrimitiveType PrimitiveLong))
        <|> (charToken *> return (PrimitiveType PrimitiveChar))
        <|> (ClassType <$> identifierToken)
        <?> "Type"

field :: Stream s Identity (SourcePos, Token) => Parsec s u Field
field = do priv <- privacyLevel
           t <- typeP
           id <- identifierToken
           semicolonToken
           return $ Field id priv t
        <?> "Field"

methodType :: Stream s Identity (SourcePos, Token) => Parsec s u (Maybe Type)
methodType = try (voidToken *> return Nothing)
                     <|> (Just <$> typeP)
                     <?> "Method Type"

methodBody :: Stream s Identity (SourcePos, Token) => Parsec s u [Statement]
methodBody = many statement

parameter :: Stream s Identity (SourcePos, Token) => Parsec s u (Type, String)
parameter = do t <- typeP
               id <- identifierToken
               return (t, id)

method :: Stream s Identity (SourcePos, Token) => Parsec s u Method
method = do priv <- privacyLevel
            t <- methodType
            id <- identifierToken
            leftParenToken
            params <- list parameter
            rightParenToken
            leftBraceToken
            body <- methodBody
            rightBraceToken
            return $ Method id priv t params body

statement :: Stream s Identity (SourcePos, Token) => Parsec s u Statement
statement = statementGroup
            <|> try declareVariable
            <|> try setVariable
            -- <|> try returnStatement
            <|> methodCallStatement
            <?> "Statement"

statementGroup :: Stream s Identity (SourcePos, Token) => Parsec s u Statement
statementGroup = leftBraceToken *> (StatementGroup <$> many statement) 
                                <* rightBraceToken

declareVariable :: Stream s Identity (SourcePos, Token) => Parsec s u Statement
declareVariable = do t <- typeP
                     id <- identifierToken
                     semicolonToken
                     return $ DeclareVariable t id

setVariable :: Stream s Identity (SourcePos, Token) => Parsec s u Statement
setVariable = do id <- identifierToken
                 equalsToken
                 expr <- expression
                 semicolonToken
                 return $ SetVariable id expr

-- | Includes semicolon
methodCallStatement :: Stream s Identity (SourcePos, Token)
                       => Parsec s u Statement
methodCallStatement = do
    (MethodCallExpression objId methodId args) <- methodCall
    semicolonToken
    return $ MethodCallStatement objId methodId args
                      

expression :: Stream s Identity (SourcePos, Token) => Parsec s u Expression
expression = buildExpressionParser table term
             <?> "Expression"

table :: Stream s Identity (SourcePos, Token)
         => [[Operator s u Identity Expression]]
table = [ [binaryMath multiplyToken Multiply, binaryMath divideToken Divide]
        , [binaryMath addToken Add, binaryMath subtractToken Subtract]
        , [binaryCompare doubleEqualsToken CEquals,
           binaryCompare greaterToken CGreater,
           binaryCompare greaterEqualsToken CGreaterEquals,
           binaryCompare lessToken CLess,
           binaryCompare lessEqualsToken CLessEquals]
        ]

binaryMath t o = Infix (t >> return (MathOperationExpression o)) AssocLeft
binaryCompare t o = Infix (t >> return (ComparisonExpression o)) AssocLeft

term :: Stream s Identity (SourcePos, Token) => Parsec s u Expression
term = parenTerm
       <|> (LiteralExpression . LiteralInt <$> try intLiteralToken)
       <|> try methodCall
       <|> (GetVariableExpression <$> identifierToken)

parenTerm :: Stream s Identity (SourcePos, Token) => Parsec s u Expression
parenTerm = leftParenToken 
                *> (ParenExpression <$> expression) 
                <* rightParenToken

-- | Doesn't include the semicolon. Probably needs its own table
-- at some point so that we can find the "dot"
methodCall :: Stream s Identity (SourcePos, Token)
                => Parsec s u Expression
methodCall = do objId <- identifierToken
                dotToken
                methodId <- identifierToken
                leftParenToken
                args <- list expression
                rightParenToken
                return $ MethodCallExpression objId methodId args

module Language.Objection.SyntaxTree
where

import Data.Int
import qualified Data.Map as M

type Identifier = String

data Module = Module (M.Map Identifier Class)
              deriving (Show, Read, Eq)

data PrivacyLevel = Private | Protected | Public
                    deriving (Show, Read, Eq)

data Primitive = PrimitiveInt
                 | PrimitiveDouble
                 | PrimitiveFloat
                 | PrimitiveLong
                 | PrimitiveChar
                 | PrimitiveBool
                 deriving (Show, Read, Eq)

-- | A type is either a primitive or the name of some other
-- class
data Type = PrimitiveType Primitive | ClassType Identifier
            deriving (Show, Read, Eq)

-- | A class consists of fields, constructors, and methods
data Class = Class Identifier PrivacyLevel [Constructor] (M.Map Identifier Field) (M.Map Identifier Method)
             deriving (Show, Read, Eq)

data Field = Field Identifier PrivacyLevel Type 
             deriving (Show, Read, Eq)

data Method = Method Identifier PrivacyLevel (Maybe Type) [(Type, Identifier)] [Statement]
              deriving (Show, Read, Eq)

data Constructor = Constructor [Type] [Statement]
                   deriving (Show, Read, Eq)

data Statement = DeclareVariable Type Identifier
                 | DeclareSetVariable Identifier Expression
                 | IfStatement Expression Statement (Maybe Statement)
                 | MethodCallStatement Identifier Identifier [Expression]
                 | Return Expression 
                 | SetVariable Identifier Expression
                 | StatementGroup [Statement]
                 deriving (Show, Read, Eq)

-- | TODO: Change MethodCallExpression to take an Expression for the client
-- object rather than an identifier.... Would be much better that way
-- (and most people would rely on this behavior)
data Expression = ComparisonExpression ComparisonOperation Expression Expression
                  | MathOperationExpression MathOperation Expression Expression
                  | MethodCallExpression Identifier Identifier [Expression]
                  | GetVariableExpression Identifier
                  | LiteralExpression Literal
                  | ParenExpression Expression
                  deriving (Show, Read, Eq)

data Literal = LiteralInt Int32
               | LiteralLong Int64
               | LiteralFloat Float
               | LiteralDouble Double
               | LiteralBool Bool
               deriving (Show, Read, Eq)

data MathOperation = Add | Subtract | Multiply | Divide
                     deriving (Show, Read, Eq)

data ComparisonOperation = CEquals 
                           | CGreater
                           | CGreaterEquals
                           | CLess
                           | CLessEquals
                           deriving (Show, Read, Eq)

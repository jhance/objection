module Language.Objection.SyntaxTree
where

import Data.Int
import qualified Data.Map as M

type Identifier = String

data Module = Module (M.Map Identifier Class)
              deriving (Show, Read)

data PrivacyLevel = Private | Protected | Public
                    deriving (Show, Read)

data Primitive = PrimitiveInt | PrimitiveLong | PrimitiveChar
                 deriving (Show, Read)

-- | A type is either a primitive or the name of some other
-- class
data Type = TypePrimitive Primitive | ClassType Identifier
            deriving (Show, Read)

-- | A class consists of fields, constructors, and methods
data Class = Class Identifier PrivacyLevel [Constructor] (M.Map Identifier Field) (M.Map Identifier Method)
             deriving (Show, Read)

data Field = Field Identifier PrivacyLevel Type 
             deriving (Show, Read)

data Method = Method Identifier PrivacyLevel (Maybe Type) [(Type, Identifier)] [Statement]
              deriving (Show, Read)

data Constructor = Constructor [Type] [Statement]
                   deriving (Show, Read)

data Statement = DeclareVariable Type Identifier
                 | DeclareSetVariable Identifier Expression
                 | MethodCallStatement Identifier Identifier [Expression]
                 | Return Expression 
                 | SetVariable Identifier Expression
                 | StatementGroup [Statement]
                 deriving (Show, Read)

-- | TODO: Change MethodCallExpression to take an Expression for the client
-- object rather than an identifier.... Would be much better that way
-- (and most people would rely on this behavior)
data Expression = MathOperationExpression MathOperation Expression Expression
                  | MethodCallExpression Identifier Identifier [Expression]
                  | GetVariableExpression Identifier
                  | IntLiteralExpression Int32
                  | ParenExpression Expression
                  deriving (Show, Read)

data MathOperation = Add | Subtract | Multiply | Divide
                     deriving (Show, Read)

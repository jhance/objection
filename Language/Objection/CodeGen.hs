module Language.Objection.CodeGen
where

import Control.Applicative
import Data.Int
import qualified Data.Map as M
import LLVM.Core hiding (Module)
import qualified LLVM.Core as L (Module)

import Language.Objection.SyntaxTree
{-
-- | A module consists of several classes, but a class is more analagous
-- to a module in the LLVM codegen stage.
convertModule :: Module -> IO [L.Module]
convertModule (Module classes) = mapM convertClass classes

-- | Converts an Objection Class to an LLVM Module
convertClass :: Class -> IO L.Module
convertClass = undefined

convertMethod :: Method -> CodeGenModule (Function (IO ())
convertMethod (Method i priv retType params statements) =
    createNamedFunction ExternalLinkage i $ do
-}

data StatementResult = 
      SRPtrInt32 (Value (Ptr Int32))
    | SRPtrInt64 (Value (Ptr Int64))
    | SRNone

data ExpressionResult = ERInt32 (Value Int32)
                        | ERInt64 (Value Int64)

convertStatements :: M.Map Identifier StatementResult
                     -> [Statement]
                     -> CodeGenFunction r StatementResult
convertStatements context [] = return SRNone
convertStatements context [st] = do (r, context') <- convertStatement context st
                                    return r
convertStatements context (st:sts) = do (r, context') <- convertStatement context st
                                        convertStatements context' sts

convertStatement :: M.Map Identifier StatementResult
                    -> Statement
                    -> CodeGenFunction r (StatementResult, 
                                          M.Map Identifier StatementResult)
convertStatement context = f where
    f (DeclareVariable t i) = 
        case t of 
            TypePrimitive PrimitiveInt -> do
                ptr <- alloca
                let r = SRPtrInt32 ptr
                    context' = M.insert i r context
                return (r, context')
            TypePrimitive PrimitiveLong -> do
                ptr <- alloca
                let r = SRPtrInt64 ptr
                    context' = M.insert i r context
                return (r, context')
        --TypePrimitive PrimitiveChar -> alloca
    f (SetVariable i e) = do
        let ptr = context M.! i
        val <- convertExpression context e
        case (ptr, val) of
            (SRPtrInt32 ptr', ERInt32 val') -> do store val' ptr'
                                                  return (SRNone, context)
            (SRPtrInt64 ptr', ERInt64 val') -> do store val' ptr'
                                                  return (SRNone, context)

-- The issue here is that a given expression can easily return
-- any number of things... a float, an int, etc. It would help
-- to have some type annotations from the non yet written
-- type analyzer to give us a type annotated expression tree.
--
-- The most problematic is the overloading on the operators...
-- Probably going to leave out float support for quite awhile.
convertExpression :: M.Map Identifier StatementResult
                     -> Expression
                     -> CodeGenFunction r ExpressionResult
convertExpression _ (IntLiteralExpression i) = return . ERInt32 . valueOf $ i
convertExpression c (ParenExpression e) = convertExpression c e
convertExpression c (MathOperationExpression op e1 e2) =
    do (ERInt32 v1) <- convertExpression c e1
       (ERInt32 v2) <- convertExpression c e2
       case op of
        Add -> ERInt32 <$> iadd v1 v2
        Subtract -> ERInt32 <$> isub v1 v2
        Multiply -> ERInt32 <$> imul v1 v2
        Divide -> ERInt32 <$> idiv v1 v2
convertExpression c (GetVariableExpression i) =
    case (c M.! i) of
        SRPtrInt32 val -> ERInt32 <$> load val
        SRPtrInt64 val -> ERInt64 <$> load val

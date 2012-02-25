module Language.Objection.CodeGen
where

import Control.Applicative
import Data.Int
import qualified Data.Map as M
import LLVM.Untyped.Core hiding (Module, Type)
import qualified LLVM.Untyped.Core as L (Module, Type)

import Language.Objection.SyntaxTree

type SymbolMap = M.Map String (Type, Value)

-- | Gets the corresponding LLVM type of a method. Currently doesn't include
-- the reference to the this object but that WILL be implemented later. Just
-- flat function.
-- typeMethod :: Method -> LLVM L.Type
-- typeMethod (Method _ _ retT params) = let (paramTypes, _) = unzip params

convertIntComparison :: ComparisonOperation -> IntComparison
convertIntComparison CEquals = IntEQ
convertIntComparison CGreater = IntSGT
convertIntComparison CGreaterEquals = IntSGE
convertIntComparison CLess = IntSLT
convertIntComparison CLessEquals = IntSLE

convertRealComparison :: ComparisonOperation -> RealComparison
convertRealCompariosn CEquals = RealOEQ
convertRealComparison CGreater = RealOGT
convertRealComparison CGreaterEquals = RealOGE
convertRealComparison CLess = RealOLT
convertRealComparison CLessEquals = RealOLE
                                
convertType :: Type -> L.Type
convertType (PrimitiveType PrimitiveInt) = int32Type

convertTypes :: [Type] -> [L.Type]
convertTypes types = map convertType types

-- | Defines a function in LLVM
defineFunction :: L.Module -> String -> [Statement] -> LLVM ()
defineFunction m name sts = do ft <- functionType int32Type [] False
                               f <- addFunction m name ft
                               entryBlock <- appendBasicBlock f ""
                               builder <- createBuilder
                               positionAtEnd builder entryBlock
                               genStatements builder f M.empty sts

genStatements :: Builder
                 -> Value
                 -> SymbolMap
                 -> [Statement]
                 -> LLVM ()
genStatements builder fn symbolMap [st] = genStatement builder fn symbolMap st
                                       >> return ()
genStatements builder fn symbolMap (st:sts) = do
    symbolMap' <- genStatement builder fn symbolMap st
    genStatements builder fn symbolMap' sts

-- | Generates code for a statement and then returns an updated symbol table
genStatement :: Builder
                -> Value -- ^ Function, used to add basic blocks
                -> SymbolMap -- ^ The current symbol table
                -> Statement -- ^ Statement for which to generate code
                -> LLVM SymbolMap -- ^ Updated symbol table

genStatement builder fn symbolMap (DeclareVariable t i) = do
    val <- buildAlloca builder (convertType t) i
    return $ M.insert i (t, val) symbolMap

genStatement builder fn symbolMap (IfStatement e trueSt falseSt) = do
    trueBlock <- appendBasicBlock fn "true"
    trueBuilder <- createBuilder
    positionAtEnd trueBuilder trueBlock

    falseBlock <- appendBasicBlock fn "false"
    falseBuilder <- createBuilder
    positionAtEnd falseBuilder falseBlock

    doneBlock <- appendBasicBlock fn "doneif"
    
    (_, exprResult) <- genExpression builder symbolMap e
    buildCondBr builder exprResult trueBlock falseBlock

    genStatement trueBuilder fn symbolMap trueSt
    buildBr trueBuilder doneBlock

    case falseSt of
        Nothing -> return ()
        Just falseSt' -> do genStatement falseBuilder fn symbolMap falseSt'
                            return ()
    buildBr falseBuilder doneBlock

    positionAtEnd builder doneBlock
    return symbolMap
genStatement builder fn symbolMap (SetVariable i e) = do
    (t, v) <- genExpression builder symbolMap e
    let (t', symbol) = symbolMap M.! i
    assertTypesEqual t t'
    buildStore builder v symbol
    return symbolMap

genStatement builder fn symbolMap (Return e) = do
    (_, v) <- genExpression builder symbolMap e
    buildRet builder v
    return symbolMap

assertTypesEqual :: Type -> Type -> LLVM ()
assertTypesEqual t1 t2 = if t1 == t2
                            then return ()
                            else error $ "CodeGen: Type " ++ (show t1) ++
                                         "does not match Type " ++ (show t2)

-- | I need to make it switch between buildICmp when I start allowing for
-- float comparison. Possible future error
genExpression :: Builder
                 -> SymbolMap
                 -> Expression
                 -> LLVM (Type, Value)
genExpression builder symbolMap = g
    where g (ComparisonExpression op e1 e2) = do
                (t, v1) <- genExpression builder symbolMap e1
                (t', v2) <- genExpression builder symbolMap e2
                assertTypesEqual t t'
                val <- if isIntType t
                        then buildICmp builder (convertIntComparison op) v1 v2 ""
                        else if isRealType t
                            then buildFCmp builder (convertRealComparison op) v1 v2 ""
                            else error "Can't compare non-integral"
                return (t, val)
          g (MathOperationExpression op e1 e2) = do
                (t, v1) <- genExpression builder symbolMap e1
                (t', v2) <- genExpression builder symbolMap e2
                assertTypesEqual t t'
                val <- if isIntType t 
                        then case op of
                            Add -> buildAdd builder v1 v2 ""
                            Subtract -> buildSub builder v1 v2 ""
                            Multiply -> buildMul builder v1 v2 ""
                        else case op of
                            Add -> buildFAdd builder v1 v2 ""
                            Subtract -> buildFSub builder v1 v2 ""
                            Multiply -> buildFMul builder v1 v2 ""
                return (t, val)
          g (GetVariableExpression i) = do
                let (t, var) = symbolMap M.! i
                val <- buildLoad builder var (i ++ "_")
                return (t, val)
          g (LiteralExpression l) = return $ genLiteral l
          g (ParenExpression e) =
                genExpression builder symbolMap e

-- | Convert an Objective Literal to an LLVM Const Value
genLiteral :: Literal -> (Type, Value)
genLiteral (LiteralInt i) =
    (PrimitiveType PrimitiveInt, constInt int32Type (fromIntegral i) True)
genLiteral (LiteralLong l) =
    (PrimitiveType PrimitiveLong, constInt int64Type (fromIntegral l) True)
genLiteral (LiteralFloat f) =
    (PrimitiveType PrimitiveFloat, constReal floatType (realToFrac f))
genLiteral (LiteralDouble d) =
    (PrimitiveType PrimitiveDouble, constReal doubleType d)
genLiteral (LiteralBool True) = 
    (PrimitiveType PrimitiveBool, constReal int1Type 1)
genLiteral (LiteralBool False) =
    (PrimitiveType PrimitiveBool, constReal int1Type 0)

isIntType :: Type -> Bool
isIntType (PrimitiveType PrimitiveInt) = True
isIntType (PrimitiveType PrimitiveLong) = True
isIntType _ = False

isRealType :: Type -> Bool
isRealType (PrimitiveType PrimitiveFloat) = True
isRealType (PrimitiveType PrimitiveDouble) = True
isRealType _ = False

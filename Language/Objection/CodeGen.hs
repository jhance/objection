module Language.Objection.CodeGen
where

import Control.Applicative
import Data.Int
import qualified Data.Map as M
import LLVM.Core hiding (Module)
import qualified LLVM.Core as L (Module)

import Language.Objection.SyntaxTree

data ReturnType =
      RInt32
    | RInt64
    | RVoid

-- | Really ought to be named to like SymbolTableEntry
data SymbolEntry = 
      SEPtrInt32 (Value (Ptr Int32))
    | SEPtrInt64 (Value (Ptr Int64))
    | SENone

data CodeGenResult a =
      CGInt32 (CodeGenFunction Int32 a)
    | CGInt64 (CodeGenFunction Int64 a)
    | CGVoid (CodeGenFunction () a)

type StatementResult = CodeGenResult (M.Map Identifier SymbolEntry)
type BlockResult = CodeGenResult ()

data ExpressionResult = ERInt32 (Value Int32)
                        | ERInt64 (Value Int64)
                        | ERBool (Value Bool)

convertStatements :: M.Map Identifier SymbolEntry
                     -> ReturnType
                     -> [Statement]
                     -> BlockResult
convertStatements context rt [st] =
    case rt of
        RInt32 -> CGInt32 $
            do let (CGInt32 cgf) = convertStatement context rt st
               cgf
               return ()
        RInt64 -> CGInt64 $
            do let (CGInt64 cgf) = convertStatement context rt st
               cgf
               return ()
        RVoid -> CGVoid $
            do let (CGVoid cgf) = convertStatement context rt st
               cgf
               return ()


convertStatements context rt (st:sts) =
    case rt of
        RInt32 -> CGInt32 $ do
            let (CGInt32 cgf) = convertStatement context rt st
            context' <- cgf
            let (CGInt32 rest) = convertStatements context' rt sts
            rest
        RInt64 -> CGInt64 $ do
            let (CGInt64 cgf) = convertStatement context rt st
            context' <- cgf
            let (CGInt64 rest) = convertStatements context' rt sts
            rest
        RVoid -> CGVoid $ do
            let (CGVoid cgf) = convertStatement context rt st
            context' <- cgf
            let (CGVoid rest) = convertStatements context' rt sts
            rest
convertStatementNonRet :: M.Map Identifier SymbolEntry
                          -> Statement
                          -> CodeGenFunction r (M.Map Identifier SymbolEntry)
convertStatementNonRet context = f
    where f (Return _) = error "Non Return Only"
          f (DeclareVariable t i) = 
            case t of 
                TypePrimitive PrimitiveInt -> do
                    ptr <- alloca
                    let r = SEPtrInt32 ptr
                        context' = M.insert i r context
                    return context'
                TypePrimitive PrimitiveLong -> do
                    ptr <- alloca
                    let r = SEPtrInt64 ptr
                        context' = M.insert i r context
                    return context'
          f (SetVariable i e) = do
            let ptr = context M.! i
            val <- convertExpression context e
            case (ptr, val) of
                (SEPtrInt32 ptr', ERInt32 val') -> do store val' ptr'
                                                      return context
                (SEPtrInt64 ptr', ERInt64 val') -> do store val' ptr'
                                                      return context

convertStatement :: M.Map Identifier SymbolEntry
                    -> ReturnType
                    -> Statement
                    -> StatementResult
convertStatement context rt = f
    where f (Return e) = case rt of
                            RInt32 -> CGInt32 $ do
                                v <- convertExpression context e
                                case v of
                                    ERInt32 v' -> do ret v'
                                                     return context
                            RInt64 -> CGInt64 $ do
                                v <- convertExpression context e
                                case v of
                                    ERInt64 v' -> do ret v'
                                                     return context
          f (StatementGroup sts) =
            case rt of
                RInt32 -> CGInt32 $ do
                    let (CGInt32 action) = convertStatements context rt sts
                    action
                    return context
                RInt64 -> CGInt64 $ do
                    let (CGInt64 action) = convertStatements context rt sts
                    action
                    return context
                RVoid -> CGVoid $ do
                    let (CGVoid action) = convertStatements context rt sts
                    action
                    return context
          f (IfStatement e trueSt falseSt) = do
            case rt of
                RInt32 -> CGInt32 $ do
                    (ERBool eResult) <- convertExpression context e
                    trueBlock <- newBasicBlock
                    falseBlock <- newBasicBlock
                    doneBlock <- newBasicBlock
                    condBr eResult trueBlock falseBlock

                    defineBasicBlock trueBlock
                    let (CGInt32 action) = convertStatement context rt trueSt
                    action
                    br doneBlock

                    defineBasicBlock falseBlock
                    case falseSt of
                        Nothing -> return ()
                        Just falseSt' ->
                            let (CGInt32 action) = convertStatement context rt falseSt'
                            in action >> return ()  
                    br doneBlock

                    defineBasicBlock doneBlock
                    return context
                RInt64 -> CGInt64 $ do
                    (ERBool eResult) <- convertExpression context e
                    trueBlock <- newBasicBlock
                    falseBlock <- newBasicBlock
                    doneBlock <- newBasicBlock
                    condBr eResult trueBlock falseBlock

                    defineBasicBlock trueBlock
                    let (CGInt64 action) = convertStatement context rt trueSt
                    action
                    br doneBlock

                    defineBasicBlock falseBlock
                    case falseSt of
                        Nothing -> return ()
                        Just falseSt' ->
                            let (CGInt64 action) = convertStatement context rt falseSt'
                            in action >> return ()
                    br doneBlock

                    defineBasicBlock doneBlock
                    return context
                RVoid -> CGVoid $ do
                    (ERBool eResult) <- convertExpression context e
                    trueBlock <- newBasicBlock
                    falseBlock <- newBasicBlock
                    doneBlock <- newBasicBlock
                    condBr eResult trueBlock falseBlock

                    defineBasicBlock trueBlock
                    let (CGVoid action) = convertStatement context rt trueSt
                    action
                    br doneBlock

                    defineBasicBlock falseBlock
                    case falseSt of
                        Nothing -> return ()
                        Just falseSt' ->
                            let (CGVoid action) = convertStatement context rt falseSt'
                            in action >> return ()
                    br doneBlock

                    defineBasicBlock doneBlock
                    return context

          -- END IF STATEMENT CODE GEN
          f st = let cgf = convertStatementNonRet context st
                 in case rt of
                    RInt32 -> CGInt32 cgf
                    RInt64 -> CGInt64 cgf
                    RVoid -> CGVoid cgf

-- The issue here is that a given expression can easily return
-- any number of things... a float, an int, etc. It would help
-- to have some type annotations from the non yet written
-- type analyzer to give us a type annotated expression tree.
--
-- The most problematic is the overloading on the operators...
-- Probably going to leave out float support for quite awhile.
convertExpression :: M.Map Identifier SymbolEntry
                     -> Expression
                     -> CodeGenFunction r ExpressionResult
convertExpression _ (IntLiteralExpression i) = return . ERInt32 . valueOf $ i
convertExpression c (ParenExpression e) = convertExpression c e
convertExpression c (MathOperationExpression op e1 e2) = do
    (ERInt32 v1) <- convertExpression c e1
    (ERInt32 v2) <- convertExpression c e2
    case op of
        Add -> ERInt32 <$> iadd v1 v2
        Subtract -> ERInt32 <$> isub v1 v2
        Multiply -> ERInt32 <$> imul v1 v2
        Divide -> ERInt32 <$> idiv v1 v2
convertExpression c (ComparisonExpression op e1 e2) = do
    (ERInt32 v1) <- convertExpression c e1
    (ERInt32 v2) <- convertExpression c e2
    let pred = case op of
            CEquals -> CmpEQ
            CGreater -> CmpGT
            CGreaterEquals -> CmpGE
            CLess -> CmpLT
            CLessEquals -> CmpLE
    ERBool <$> cmp pred v1 v2
convertExpression c (GetVariableExpression i) =
    case (c M.! i) of
        SEPtrInt32 val -> ERInt32 <$> load val
        SEPtrInt64 val -> ERInt64 <$> load val

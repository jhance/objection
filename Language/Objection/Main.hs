module Main
where

import Data.Int
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.IO
import Text.Parsec.Text.Lazy

import LLVM.Core hiding (Module)
import qualified LLVM.Core as L (Module)

import Language.Objection.CodeGen
import Language.Objection.Parser
import Language.Objection.SyntaxTree

sts = [DeclareVariable (TypePrimitive PrimitiveInt) "awesome",
       DeclareVariable (TypePrimitive PrimitiveInt) "awesome2",
       SetVariable "awesome" (IntLiteralExpression 5),
       SetVariable "awesome2" (MathOperationExpression Multiply
                                (GetVariableExpression "awesome")
                                (IntLiteralExpression 2)
                                ),
       DeclareVariable (TypePrimitive PrimitiveInt) "result",
       IfStatement
        (ComparisonExpression CLess (GetVariableExpression "awesome") (GetVariableExpression "awesome2"))
        (SetVariable "result" (GetVariableExpression "awesome"))
        (Just (SetVariable "result" (GetVariableExpression "awesome2"))),
       Return (GetVariableExpression "awesome2")
       ]

mTest :: CodeGenModule (Function (IO Int32))
mTest = createFunction ExternalLinkage $ do
    let (CGInt32 code) = convertStatements M.empty RInt32 sts
    code

main :: IO ()
main = do handle <- openFile "input.objection" ReadMode
          contents <- T.hGetContents handle
          print $ parseModule "input.objection" contents

          initializeNativeTarget
          m <- newNamedModule "main"
          defineModule m mTest
          writeBitcodeToFile "out.bc" m

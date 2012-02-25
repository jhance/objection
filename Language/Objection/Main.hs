module Main
where

import Data.Int
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.IO
import Text.Parsec.Text.Lazy

import LLVM.Untyped.Core

import Language.Objection.CodeGen
import Language.Objection.Parser
import Language.Objection.SyntaxTree

sts = [DeclareVariable (PrimitiveType PrimitiveInt) "awesome",
       DeclareVariable (PrimitiveType PrimitiveInt) "awesome2",
       SetVariable "awesome" (LiteralExpression (LiteralInt 5)),
       SetVariable "awesome2" (MathOperationExpression Multiply
                                (GetVariableExpression "awesome")
                                (LiteralExpression (LiteralInt 5))
                                ),
       IfStatement (ComparisonExpression CGreater -- Minimum Function
                    (GetVariableExpression "awesome")
                    (GetVariableExpression "awesome2"))
                   (SetVariable "awesome"
                    (GetVariableExpression ("awesome2")))
                   (Just (SetVariable "awesome2"
                    (GetVariableExpression ("awesome")))),
       Return (GetVariableExpression "awesome2")
       ]

main :: IO Bool
main = do handle <- openFile "input.objection" ReadMode
          contents <- T.hGetContents handle
          print $ parseModule "input.objection" contents

          mod <- runLLVM $ do
                m <- moduleCreateWithName "main"
                defineFunction m "testFunction" sts
                return m

          writeBitcodeToFile mod "out.bc"

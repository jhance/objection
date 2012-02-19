module Language.Objection.TypeCheck
    (typeCheck)
where

import qualified Data.Map as M

import Language.Objection.SyntaxTree

data TypeCheckResult = TypeCheckSuccess
                       | TypeCheckFailure String
                       deriving (Read, Show)

typeCheck :: Module -> TypeCheckResult
typeCheck (Module classes) = TypeCheckSuccess

lookupFieldType :: Module 
                   -> Identifier -- ^ Class Identifier
                   -> Identifier -- ^ Field Identifier
                   -> Maybe Type
lookupFieldType = undefined

lookupMethodType :: Module
                    -> Identifier
                    -> Identifier
                    -> Maybe (Maybe Type, [Type])
lookupMethodType = undefined

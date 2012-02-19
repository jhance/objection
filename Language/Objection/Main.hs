module Main
where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.IO
import Text.Parsec.Text.Lazy

import Language.Objection.Parser

main :: IO ()
main = do handle <- openFile "input.objection" ReadMode
          contents <- T.hGetContents handle
          print $ parseModule "input.objection" contents

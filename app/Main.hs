{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import TypedFun
import Expr
import Parser

main :: IO ()
--main = putStrLn (show . evaluate . parseExprTest ex)
main = parseExprTest ex


ex :: Text
ex = "let f x: int -> int = x + 1 in if 1=1 then 3 else f 4"

ex1 :: TypedExpr
ex1 = 
  LetFun 
    "f"
    "x" 
    TypI 
    (Prim "+" (Var "x") (CstI 1)) 
    TypI 
    (Let "y" (Call (Var "f") (CstI 1)) (Call (Var "f") (Var "y")))

ex2 :: TypedExpr
ex2 =
  LetFun
    "fac"
    "x"
    TypI
    (If (Prim "=" (Var "x") (CstI 0))
        (CstI 1)
        (Prim "*" (Var "x") (Call (Var "fac") (Prim "-" (Var "x") (CstI 1)))))
    TypI
    (Let "n" (CstI 7) (Call (Var "fac") (Var "n")))

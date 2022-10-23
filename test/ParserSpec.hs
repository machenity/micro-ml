{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec
import Expr
import Parser (parseCode)

spec :: Spec
spec = describe "Parser" $ do
  it "parses let-binding" $ do
    parseCode "" "let x = 1 in x + y" `shouldBe` Right (Let "x" (CstI 1) (Prim "+" (Var "x") (Var "y")))
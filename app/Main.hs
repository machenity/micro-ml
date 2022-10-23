{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.IO as TextIO
import TypedFun
import Expr
import Parser

main :: IO ()
--main = putStrLn (show . evaluate . parseExprTest ex)
main = do {
  code <- TextIO.getLine;
  parseExprTest code;
}

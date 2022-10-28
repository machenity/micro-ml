module Main (main) where

import qualified Data.Text.IO as TextIO

import Parser

main :: IO ()
--main = putStrLn (show . evaluate . parseExprTest ex)
main = do {
  code <- TextIO.getLine;
  parseExprTest code;
}

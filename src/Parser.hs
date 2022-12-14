{-# LANGUAGE OverloadedStrings #-}

module Parser (parseExpr, parseCode, parseExprTest) where

import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Text (Text)
import Data.Void (Void)
import Expr (Typ (TypB, TypI), TypedExpr (..), FunDef(..))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- L.space 는 공백문자를 인식하는 파서
-- space1은 Text.Megaparsec.Char 의 요구사항에 부합하는 함수

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

reserved :: Text -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

reservedWords :: [String]
reservedWords = ["let", "in", "end", "if", "then", "else", "true", "false", "int", "bool", ":", "->", "="]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` reservedWords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

parseType :: Parser Typ
parseType = (TypI <$ reserved "int") <|> (TypB <$ reserved "bool")

operators :: [[Operator Parser TypedExpr]]
operators =
  [ [ InfixL (Prim "*" <$ symbol "*"),
      InfixL (Prim "/" <$ symbol "/")
    ],
    [ InfixL (Prim "+" <$ symbol "+"),
      InfixL (Prim "-" <$ symbol "-")
    ],
    [ InfixL (Prim "=" <$ symbol "="),
      InfixL (Prim "<" <$ symbol "<")
    ],
    [InfixL (Prim "&" <$ symbol "&")]
  ]

term :: Parser TypedExpr
term =
  parens parseExpr
    <|> try callExpr
    <|> (CstI <$> integer)
    <|> (CstB True <$ reserved "true")
    <|> (CstB False <$ reserved "false")
    <|> (Var <$> identifier)
    <?> "term"

expr :: Parser TypedExpr
expr = makeExprParser term operators

parseExpr :: Parser TypedExpr
parseExpr =
  ifThenElse
    <|> try letFunExpr
    <|> letExpr
    -- <|> try callExpr
    <|> expr

ifThenElse :: Parser TypedExpr
ifThenElse = do
  reserved "if"
  cond <- parseExpr
  reserved "then"
  thenExpr <- parseExpr
  reserved "else"
  If cond thenExpr <$> parseExpr

letExpr :: Parser TypedExpr
letExpr = do
  reserved "let"
  var <- identifier
  reserved "="
  val <- parseExpr
  reserved "in"
  Let var val <$> parseExpr

letFunExpr :: Parser TypedExpr
letFunExpr = do
  reserved "let"
  funDefs <- funDef `sepBy1` symbol ";"
  reserved "in"
  LetFun funDefs <$> parseExpr

funDef :: Parser FunDef
funDef = do
  funName <- identifier
  argNames <- some identifier
  _ <- symbol ":"
  argTypes <- parens $ some parseType
  _ <- symbol "->"
  returnType <- parseType
  _ <- symbol "="
  funBody <- parseExpr
  return $ FunDef funName argNames argTypes funBody returnType

callExpr :: Parser TypedExpr
callExpr = do
  f <- identifier
  args <- some parseExpr
  return (Call (Var f) args)

parseExprTest :: Text -> IO ()
parseExprTest = parseTest (parseExpr <* eof)

parseCode :: String -> Text -> Either (ParseErrorBundle Text Void) TypedExpr
parseCode = parse (parseExpr <* eof)

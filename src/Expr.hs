module Expr (Env, Typ (..), TypedExpr (..)) where

type Env v = [(String, v)]

data Typ
  = TypI
  | TypB
  | TypF [Typ] Typ
  deriving (Eq, Show)

data TypedExpr
  = CstI Integer
  | CstB Bool
  | Var String
  | Let String TypedExpr TypedExpr
  | Prim String TypedExpr TypedExpr
  | If TypedExpr TypedExpr TypedExpr
  | LetFun String [String] [Typ] TypedExpr Typ TypedExpr
  | --      (f,     x,   xTyp, fBody,    rTyp, letBody)
    Call TypedExpr [TypedExpr]
  deriving (Eq, Show)

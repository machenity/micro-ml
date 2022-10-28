module Expr (Env, Typ (..), TypedExpr (..), FunDef(..)) where

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
  | LetFun [FunDef] TypedExpr
  | Call TypedExpr [TypedExpr]
  deriving (Eq, Show)

--                   (f,    x,       xTyp, fBody,    rTyp)
data FunDef = FunDef String [String] [Typ] TypedExpr Typ 
  deriving (Eq, Show)
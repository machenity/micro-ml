module Eval (evaluate) where

import Expr (Env, TypedExpr (..), FunDef(..), lookup')

data Value
  = Int Integer
  | Closure String [String] TypedExpr (Env Value)
  deriving (Show)

eval :: TypedExpr -> Env Value -> Value
eval (CstI i) _ = Int i
eval (CstB b) _ = Int $ if b then 1 else 0
eval (Var x) env = lookup' env x
eval (Prim op e1 e2) env = Int $ case (op, eval e1 env, eval e2 env) of
  ("+", Int i1, Int i2) -> i1 + i2
  ("-", Int i1, Int i2) -> i1 - i2
  ("*", Int i1, Int i2) -> i1 * i2
  ("=", Int i1, Int i2) -> if i1 == i2 then 1 else 0
  ("<", Int i1, Int i2) -> if i1 < i2 then 1 else 0
  _ -> error "unknown primitive or wrong type"
eval (Let x eRhs letBody) env =
  let xVal = eval eRhs env
      bodyEnv = (x, xVal) : env
   in eval letBody bodyEnv
eval (If e1 e2 e3) env =
  case eval e1 env of
    Int 0 -> eval e3 env
    Int _ -> eval e2 env
    _ -> error "eval If"
eval (LetFun funDefs letBody) env =
  let envWithDefs = map (\(FunDef f xs _ fBody _) -> (f, Closure f xs fBody envWithDefs)) funDefs ++ env
   in eval letBody envWithDefs
eval (Call eFun eArgs) env =
  let fClosure = eval eFun env
   in case fClosure of
      Closure f xs fBody fDeclEnv ->
        let xVals = map (`eval` env) eArgs
            argEnv = zip xs xVals
            fBodyEnv = argEnv ++ (f, fClosure) : fDeclEnv
            -- fBodyEnv = argEnv ++ fDeclEnv
         in eval fBody fBodyEnv
      _ -> error "eval Call: not a function"

evaluate :: TypedExpr -> Value
evaluate e = eval e []

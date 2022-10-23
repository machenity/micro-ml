module TypedFun (evaluate, typeCheck) where

import Expr (Env, Typ (..), TypedExpr (..))

lookup' :: Env v -> String -> v
lookup' env x = case lookup x env of
  Just v -> v
  Nothing -> error ("unbound variable " ++ x)

data Value
  = Int Integer
  | Closure String [String] TypedExpr (Env Value)

eval :: TypedExpr -> Env Value -> Integer
eval (CstI i) _ = i
eval (CstB b) _ = if b then 1 else 0
eval (Var x) env = case lookup' env x of
  Int i -> i
  _ -> error "eval Var"
eval (Let x eRhs letBody) env =
  let xVal = Int (eval eRhs env)
      bodyEnv = (x, xVal) : env
   in eval letBody bodyEnv
eval (Prim op e1 e2) env = case (op, eval e1 env, eval e2 env) of
  ("+", i1, i2) -> i1 + i2
  ("-", i1, i2) -> i1 - i2
  ("*", i1, i2) -> i1 * i2
  ("=", i1, i2) -> if i1 == i2 then 1 else 0
  ("<", i1, i2) -> if i1 < i2 then 1 else 0
  _ -> error "eval Prim"
eval (If e1 e2 e3) env =
  if eval e1 env /= 0
    then eval e2 env
    else eval e3 env
eval (LetFun f xs _ fBody _ letBody) env =
  let bodyEnv = (f, Closure f xs fBody env) : env
   in eval letBody bodyEnv
eval (Call (Var fName) eArgs) env =
  let fClosure = lookup' env fName
   in case fClosure of
        Closure f xs fBody fDeclEnv ->
          let xVals = map (\arg -> Int (eval arg env)) eArgs
              argEnv = zip xs xVals
              fBodyEnv = argEnv ++ (f, fClosure) : fDeclEnv
           in eval fBody fBodyEnv
        _ -> error "eval Call: not a function"
eval (Call _ _) _ = error "eval Call: not first-order function"

evaluate :: TypedExpr -> Integer
evaluate e = eval e []

typ :: TypedExpr -> Env Typ -> Typ
typ (CstI _) _ = TypI
typ (CstB _) _ = TypB
typ (Var x) env = lookup' env x
typ (Prim ope e1 e2) env =
  let t1 = typ e1 env
      t2 = typ e2 env
   in case (ope, t1, t2) of
        ("+", TypI, TypI) -> TypI
        ("-", TypI, TypI) -> TypI
        ("*", TypI, TypI) -> TypI
        ("=", TypI, TypI) -> TypB
        ("<", TypI, TypI) -> TypB
        ("&", TypB, TypB) -> TypB
        _ -> error "unknown op, or type error"
typ (Let x eRhs letBody) env =
  let xTyp = typ eRhs env
      letBodyEnv = (x, xTyp) : env
   in typ letBody letBodyEnv
typ (If e1 e2 e3) env =
  let t1 = typ e1 env
      t2 = typ e2 env
      t3 = typ e3 env
   in if t1 == TypB && t2 == t3
        then t2
        else error "type error in If"
typ (LetFun f xs xTyps fBody rTyp letBody) env =
  if length xs /= length xTyps
    then error "Count"
    else
      let fTyp = TypF xTyps rTyp
          argTyps = zip xs xTyps
          fBodyEnv = argTyps ++ (f, fTyp) : env
          letBodyEnv = (f, fTyp) : env
       in if typ fBody fBodyEnv == rTyp
            then typ letBody letBodyEnv
            else error "type error in function body"
typ (Call (Var fName) eArgs) env = case lookup' env fName of
  (TypF xTyps rTyp) ->
    if all (\t -> typ (fst t) env == snd t) (zip eArgs xTyps)
      then rTyp
      else error "type error in function argument"
  _ -> error "not a function"
typ _ _ = error "type error"

typeCheck :: TypedExpr -> Typ
typeCheck e = typ e []


module TypedFun (evaluate, typeCheck) where

import Expr (Env, Typ (..), TypedExpr (..), FunDef(..))

lookup' :: Env v -> String -> v
lookup' env x = case lookup x env of
  Just v -> v
  Nothing -> error ("unbound variable " ++ x)

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
typ (LetFun funDefs letBody) env =
  if any (\(FunDef _ xs xTyps _ _) -> length xs /= length xTyps) funDefs
    then error "Count"
    else
      let fTypsByf = map (\(FunDef f _ xTyps _ rTyp) -> (f, TypF xTyps rTyp)) funDefs
          fBodiesAndEnvsAndRTyps = map (\(FunDef _ xs xTyps fBody rTyp) -> (fBody, zip xs xTyps ++ fTypsByf ++ env, rTyp)) funDefs
          letBodyEnv = fTypsByf ++ env
       in if all (\(fBody, fBodyEnv, rTyp) -> typ fBody fBodyEnv == rTyp) fBodiesAndEnvsAndRTyps
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

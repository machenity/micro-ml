module Typing(typeCheck) where

import Expr ( Env, FunDef(FunDef), Typ(..), TypedExpr(..), lookup' )

typeCheck :: TypedExpr -> Typ
typeCheck e = typ e []

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

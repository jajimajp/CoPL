open Syntax

type state =
    Ret of value
  | EK of env * exp * cont
  | KV of cont * value

let eval_e_k (env, exp, cont) = 
  match exp with
    Int i -> KV (cont, IntV i) (* E-Int *)
  | Bool b -> KV (cont, BoolV b) (* E-Bool *)
  | IfExp (exp1, exp2, exp3) -> (* E-If *)
      EK (env, exp1, IfE (env, exp2, exp3, cont))
  | BinOp (op, exp1, exp2) -> (* E-BinOp *)
      EK (env, exp1, OpE (env, op, exp2, cont))
  | Var x -> (* E-Var *)
      let value = List.assoc x env in 
      KV (cont, value)
  | LetExp (x, exp1, exp2) -> (* E-Let *)
      let newcont = LetE (env, x, exp2, cont) in
      EK (env, exp1, newcont)
  | FunExp (x, exp) -> (* E-Fun *)
      let value = FunV (env, x, exp) in
      KV (cont, value)
  | AppExp (exp1, exp2) -> (* E-App *)
      let newcont = AppE (env, exp2, cont) in
      EK (env, exp1, newcont)
  | LetRecExp (x, y, exp1, exp2) -> (* E-LetRec *)
      let newenv = (x, RecV (env, x, y, exp1)) :: env in
      EK (newenv, exp2, cont)
  | Nil -> (* E-Nil *)
      KV (cont, NilV)
  | Cons (exp1, exp2) -> (* E-Cons *)
      let newcont = ConsE (env, exp2, cont) in
      EK (env, exp1, newcont)
  | MatchExp (exp1, exp2, x, y, exp3) -> (* E-Match *)
      let newcont = MatchE (env, exp2, x, y, exp3, cont) in
      EK (env, exp1, newcont)
  | LetCC (x, exp) -> (* E-LetCC *)
      let newenv = (x, ContV cont) :: env in
      EK (newenv, exp, cont)

let eval_k_v (cont, value) =
  match cont, value with
    End, _ -> Ret value (* C-Ret *)
  | OpE (env, op, exp, cont), _ -> (* C-EvalR *)
      EK (env, exp, OpV (op, value, cont))
  | OpV (Plus, IntV i1, cont), IntV i2 -> (* C-Plus *)
      KV (cont, IntV (i1 + i2))
  | OpV (Minus, IntV i1, cont), IntV i2 -> (* C-Minus *)
      KV (cont, IntV (i1 - i2))
  | OpV (Mult, IntV i1, cont), IntV i2 -> (* C-Times *)
      KV (cont, IntV (i1 * i2))
  | OpV (Lt, IntV i1, cont), IntV i2 -> (* C-Lt *)
      KV (cont, BoolV (i1 < i2))
  | IfE (env, exp1, exp2, cont), BoolV true -> (* C-IfT *)
      EK (env, exp1, cont)
  | IfE (env, exp1, exp2, cont), BoolV false -> (* C-IfF *)
      EK (env, exp2, cont)
  | LetE (env, id, exp, cont), value -> (* C-LetBody *)
      let newenv = (id, value) :: env in
      EK (newenv, exp, cont)
  | AppE (env, exp, cont), value -> (* C-EvalArg *)
      let newcont = AppV (value, cont) in
      EK (env, exp, newcont)
  | AppV (FunV (env, id, exp), cont), value -> (* C-EvalFun *)
      let newenv = (id, value) :: env in
      EK (newenv, exp, cont)
  | AppV (RecV (env, x, y, exp), cont), value -> (* C-EvalFunR *)
      let newenv = (y, value) :: (x, RecV (env, x, y, exp)) :: env in
      EK (newenv, exp, cont)
  | ConsE (env, exp, cont), value -> (* C-EvalConsR *)
      let newcont = ConsV (value, cont) in
      EK (env, exp, newcont)
  | ConsV (v1, cont), v2 -> (* C-Cons *)
      KV (cont, ConsV (v1, v2))
  | MatchE (env, exp1, x, y, exp2, cont), NilV -> (* C-MatchNil *)
      EK (env, exp1, cont)
  | MatchE (env, exp1, x, y, exp2, cont), ConsV (v1, v2) -> (* C-MatchCons *)
      let newenv = (y, v2) :: (x, v1) :: env in
      EK (newenv, exp2, cont)
  | AppV (ContV k1, k2), v1 -> (* C-EvalFunC *)
      KV (k1, v1)



let rec eval_st = function
    Ret v -> v
  | EK (env, e, k) -> eval_st (eval_e_k (env, e, k))
  | KV (k, v) -> eval_st (eval_k_v (k, v))

let env = []
let e =
  BinOp (Plus,
    BinOp (Plus, Int 1, Int 2),
    BinOp (Plus,
      LetCC ("f",
        BinOp (Plus,
          BinOp (Plus,
            AppExp (Var "f", BinOp(Plus, Int 1, Int 2)),
            Int 4),
          Int 5)
      ),
      Int 6)
  )

let st = EK (env, e, End)

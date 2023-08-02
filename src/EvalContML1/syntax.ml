type op = Plus | Minus | Mult | Lt

type exp =
    Int of int
  | Bool of bool
  | BinOp of op * exp * exp 
  | IfExp of exp * exp * exp

type value = IntV of int | BoolV of bool

(* 継続 *)
type cont =
    End
  | OpE of op * exp * cont       (* {_ op e} >> k *)
  | OpV of op * value * cont     (* {v op _} >> k *)
  | IfE of exp * exp * cont (* {if _ then e else e} >> k *)

type state =
    Ret of value
  | EK of exp * cont
  | KV of cont * value

let eval_e_k (exp, cont) = 
  match exp with
    Int i -> KV (cont, IntV i)
  | Bool b -> KV (cont, BoolV b)
  | BinOp (op, exp1, exp2) ->
      EK (exp1, OpE (op, exp2, cont))
  | IfExp (exp1, exp2, exp3) ->
      EK (exp1, IfE (exp2, exp3, cont))

let eval_k_v (cont, value) =
  match cont, value with
    End, _ -> Ret value
  | OpE (op, exp, cont), _ -> EK (exp, OpV (op, value, cont))
  | OpV (Plus, IntV i1, cont), IntV i2 ->
      KV (cont, IntV (i1 + i2))
  | OpV (Minus, IntV i1, cont), IntV i2 ->
      KV (cont, IntV (i1 - i2))
  | OpV (Mult, IntV i1, cont), IntV i2 ->
      KV (cont, IntV (i1 * i2))
  | OpV (Lt, IntV i1, cont), IntV i2 ->
      KV (cont, BoolV (i1 < i2))
  | IfE (exp1, exp2, cont), BoolV true ->
      EK (exp1, cont)
  | IfE (exp1, exp2, cont), BoolV false ->
      EK (exp2, cont)

let rec eval_st = function
    Ret v -> v
  | EK (e, k) -> eval_st (eval_e_k (e, k))
  | KV (k, v) -> eval_st (eval_k_v (k, v))

let e =
  IfExp (BinOp (Lt, Int 1, Int 2),
    Int 1,
    Int 2)
let st = EK (e, End)

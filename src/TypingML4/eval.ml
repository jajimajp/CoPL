open Syntax

type exval =
    IntV of int
  | BoolV of bool
  | FunV of exval Environment.env * id * exp
  | RecV of exval Environment.env * id * id * exp
  | NilV
  | ConsV of exval * exval

let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | FunV (env, id, exp) ->
      let envstr = string_of_envlist (Environment.to_list env) in
      let s = string_of_exp exp in
      "(" ^ envstr ^ ")[fun " ^ id ^ " -> " ^ s ^ "]"
  | RecV (env, x, y, exp) ->
      let envstr = string_of_envlist (Environment.to_list env) in
      let s = string_of_exp exp in
      "(" ^ envstr ^ ")[rec " ^ x ^ " = fun " ^ y ^ " -> " ^ s ^ "]"
  | NilV -> "[]"
  | ConsV (v1, v2) ->
      let s1 = string_of_exval v1 in
      let s2 = string_of_exval v2 in
      "(" ^ s1 ^ " :: " ^ s2 ^ ")"
and string_of_envlist = function
    [] -> ""
  | (k, v)::[] ->
      let vs = string_of_exval v in
      k ^ " = " ^ vs
  | (k, v)::tl ->
      let tls = string_of_envlist tl in
      let vs = string_of_exval v in
      tls ^ ", " ^ k ^ " = " ^ vs

let eval_prim op i1 i2 =
  match op with
    Plus -> IntV (i1 + i2)
  | Minus -> IntV (i1 - i2)
  | Times -> IntV (i1 * i2)
  | Lt -> BoolV (i1 < i2)

let rec eval_exp env = function
    Int i -> IntV i
  | Bool b -> BoolV b
  | Var id -> Environment.assoc id env
  | BinOp (op, exp1, exp2) ->
      let IntV i1 = eval_exp env exp1 in
      let IntV i2 = eval_exp env exp2 in
      eval_prim op i1 i2
  | IfExp (exp1, exp2, exp3) ->
      (match eval_exp env exp1 with
         BoolV cond ->
           if cond then eval_exp env exp2
           else eval_exp env exp3
       | _ -> failwith "IfExp got non-bool expression")
  | LetExp (id, exp1, exp2) ->
      let v1 = eval_exp env exp1 in
      let newenv = Environment.extend id v1 env in
      eval_exp newenv exp2
  | FunExp (id, exp) ->
      FunV (env, id, exp)
  | AppExp (exp1, exp2) ->
      let v1 = eval_exp env exp1 in
      (match v1 with
         FunV (fenv, id, exp) ->
           let idval = eval_exp env exp2 in
           let newenv = Environment.extend id idval fenv in
           eval_exp newenv exp
       | RecV (fenv, x, y, exp) ->
           let v2 = eval_exp env exp2 in
           let env = Environment.extend x v1 fenv in
           let env = Environment.extend y v2 env in
           eval_exp env exp 
       | _ -> failwith "Invalid input")
  | LetRecExp (x, y, exp1, exp2) ->
      let env = Environment.extend x (RecV (env, x, y, exp1)) env in
      eval_exp env exp2
  | Nil -> NilV
  | Cons (exp1, exp2) ->
      let v1 = eval_exp env exp1 in
      let v2 = eval_exp env exp2 in
      ConsV (v1, v2)
  | MatchExp (exp1, exp2, x, y, exp3) -> 
      let v1 = eval_exp env exp1 in
      (match v1 with
         NilV -> eval_exp env exp2
       | ConsV (v1, v2) ->
           let newenv = Environment.extend y v2 (
                          Environment.extend x v1 env
                        ) in
           eval_exp newenv exp2
       | _ -> failwith "Invalid value for MatchExp")

(*
let env = Environment.empty
let t =
  LetRecExp("fact", "n",
    IfExp (BinOp(Lt, Var "n", Int 1),
      Int 1,
      BinOp(Times,
        Var "n",
        AppExp(Var "fact", BinOp(Minus, Var "n", Int 1))
      )
    ),
    AppExp (Var "fact", Int 5)
  )
let _ = print_string (string_of_exval (eval_exp env t))
*)



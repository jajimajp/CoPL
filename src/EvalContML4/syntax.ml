type op = Plus | Minus | Mult | Lt

type id = string

type exp =
    Int of int
  | Bool of bool
  | Var of id
  | BinOp of op * exp * exp 
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | FunExp of id * exp
  | AppExp of exp * exp
  | LetRecExp of id * id * exp * exp
  | Nil
  | Cons of exp * exp
  | MatchExp of exp * exp * id * id * exp
  | LetCC of id * exp

and value =
    IntV of int
  | BoolV of bool
  | FunV of env * id * exp
  | RecV of env * id * id * exp
  | NilV
  | ConsV of value * value
  | ContV of cont (* 第一級継続 *)

and env = (id * value) list

(* 継続 *)
and cont =
    End
  | OpE of env * op * exp * cont       (* {_ op e} >> k *)
  | OpV of op * value * cont     (* {v op _} >> k *)
  | IfE of env * exp * exp * cont (* {if _ then e else e} >> k *)
  | LetE of env * id * exp * cont
  | AppE of env * exp * cont
  | AppV of value * cont
  | ConsE of env * exp * cont
  | ConsV of value * cont
  | MatchE of env * exp * id * id * exp * cont

let string_of_op = function
    Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Lt -> "<"

let rec string_of_exp = function
    Int i -> "(" ^ string_of_int i ^ ")"
  | Bool b -> string_of_bool b
  | Var id -> id
  | BinOp (op, exp1, exp2) ->
      let s1 = string_of_exp exp1 in
      let s2 = string_of_exp exp2 in
      let ops = string_of_op op in
      "(" ^ s1 ^ " " ^ ops ^ " " ^ s2 ^ ")"
  | IfExp (exp1, exp2, exp3) ->
      let s1 = string_of_exp exp1 in
      let s2 = string_of_exp exp2 in
      let s3 = string_of_exp exp3 in
      "(if " ^ s1 ^ " then " ^ s2 ^ " else " ^ s3 ^ ")"
  | LetExp (x, exp1, exp2) ->
      let s1 = string_of_exp exp1 in
      let s2 = string_of_exp exp2 in
      "(let " ^ x ^ " = " ^ s1 ^ " in " ^ s2 ^ ")"
  | FunExp (x, exp) ->
      let s = string_of_exp exp in
      "(fun " ^ x ^ " -> " ^ s ^ ")"
  | AppExp (exp1, exp2) ->
      let s1 = string_of_exp exp1 in
      let s2 = string_of_exp exp2 in
      "(" ^ s1 ^ " " ^ s2 ^ ")"
  | LetRecExp (x, y, exp1, exp2) ->
      let s1 = string_of_exp exp1 in
      let s2 = string_of_exp exp2 in
      "(let rec " ^ x ^ " = fun " ^ y ^ " -> " ^ s1 ^ " in " ^ s2 ^ ")"
  | Nil -> "[]"
  | Cons (exp1, exp2) ->
      let s1 = string_of_exp exp1 in
      let s2 = string_of_exp exp2 in
      "(" ^ s1 ^ " :: " ^ s2 ^ ")"
  | MatchExp (exp1, exp2, x, y, exp3) ->
      let s1 = string_of_exp exp1 in
      let s2 = string_of_exp exp2 in
      let s3 = string_of_exp exp3 in
      "(match " ^ s1 ^ " with [] -> " ^ s2 ^ " | " ^ x ^ " :: " ^ y ^ " -> " ^ s3 ^ ")"
  | LetCC (x, exp) ->
      let s = string_of_exp exp in
      "(letcc " ^ x ^ " in " ^ s ^ ")"

let rec string_of_value = function
    IntV i -> "(" ^ string_of_int i ^ ")"
  | BoolV b -> string_of_bool b
  | FunV (env, x, exp) ->
      let envs = string_of_env env in
      let s = string_of_exp exp in
      "(" ^ envs ^ ")[fun " ^ x ^ " -> " ^ s ^ "]"
  | RecV (env, x, y, exp) ->
      let envs = string_of_env env in
      let s = string_of_exp exp in
      "(" ^ envs ^ ")[rec " ^ x ^ " = fun " ^ y ^ " -> " ^ s ^ "]"
  | NilV -> "[]"
  | ConsV (v1, v2) ->
      let s1 = string_of_value v1 in
      let s2 = string_of_value v2 in
      "(" ^ s1 ^ " :: " ^ s2 ^ ")"
  | ContV cont ->
      let s = string_of_cont cont in
      "[" ^ s ^ "]"

and string_of_env = function
    [] -> ""
  | (id, v)::[] ->
      let vs = string_of_value v in
      id ^ " = " ^ vs
  | (id, v)::rest ->
      let vs = string_of_value v in
      string_of_env rest ^ ", " ^ id ^ " = " ^ vs

and string_of_cont = function
    End -> "_"
  | OpE (env, op, exp, cont) ->
      let envs = string_of_env env in
      let ops = string_of_op op in
      let s = string_of_exp exp in
      let conts = string_of_cont cont in
      "{" ^ envs ^ " |- _ " ^ ops ^ " " ^ s ^ "} >> " ^ conts
  | OpV (op, value, cont) ->
      let ops = string_of_op op in
      let vs = string_of_value value in
      let conts = string_of_cont cont in
      "{" ^ vs ^ " " ^ ops ^ " _} >> " ^ conts
  | IfE (env, exp1, exp2, cont) ->
      let envs = string_of_env env in
      let s1 = string_of_exp exp1 in
      let s2 = string_of_exp exp2 in
      let conts = string_of_cont cont in
      "{" ^ envs ^ " |- if _ then " ^ s1 ^ " else " ^ s2 ^ "} >> " ^ conts
  | LetE (env, x, exp, cont) ->
      let envs = string_of_env env in
      let s = string_of_exp exp in
      let conts = string_of_cont cont in
      "{" ^ envs ^ " |- let " ^ x ^ " = _ in " ^ s ^ "} >> " ^ conts
  | AppE (env, exp, cont) ->
      let envs = string_of_env env in
      let s = string_of_exp exp in
      let conts = string_of_cont cont in
      "{" ^ envs ^ " |- _ " ^ s ^ "} >> " ^ conts
  | AppV (value, cont) ->
      let vs = string_of_value value in
      let conts = string_of_cont cont in
      "{" ^ vs ^ " _} >> " ^ conts
  | ConsE (env, exp, cont) ->
      let envs = string_of_env env in
      let s = string_of_exp exp in
      let conts = string_of_cont cont in
      "{" ^ envs ^ " |- _ :: " ^ s ^ "} >> " ^ conts
  | ConsV (value, cont) ->
      let vs = string_of_value value in
      let conts = string_of_cont cont in
      "{" ^ vs ^ " :: _} >> " ^ conts
  | MatchE (env, exp1, x, y, exp2, cont) ->
      let envs = string_of_env env in
      let s1 = string_of_exp exp1 in
      let s2 = string_of_exp exp2 in
      let conts = string_of_cont cont in
      "{" ^ envs ^ " |- match _ with [] -> " ^ s1
          ^ " | " ^ x ^ " :: " ^ y ^ " -> " ^ s2 ^ "} >> " ^ conts

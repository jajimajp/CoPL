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
  | RefExp of exp
  | Deref of exp
  | Assign of exp * exp

and loc = int

and value =
    IntV of int
  | BoolV of bool
  | LocV of loc
  | FunV of env * id * exp
  | RecV of env * id * id * exp

and env = (id * value) list

and store = (loc * value) list

let string_of_op = function
    Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Lt -> "<"

let rec string_of_exp = function
    Int i ->
      if i < 0 then "(" ^ string_of_int i ^ ")"
      else string_of_int i
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
  | RefExp exp ->
      "(ref " ^ string_of_exp exp ^ ")"
  | Deref exp ->
      "(!" ^ string_of_exp exp ^ ")"
  | Assign (exp1, exp2) -> 
      let s1 = string_of_exp exp1 in
      let s2 = string_of_exp exp2 in
      "(" ^ s1 ^ " := " ^ s2 ^ ")"

let rec string_of_value = function
    IntV i ->
      if i < 0 then "(" ^ string_of_int i ^ ")"
      else string_of_int i
  | BoolV b -> string_of_bool b
  | LocV l ->
      let lstr =
        if l = 0 then "l" else "l" ^ (string_of_int l) in
        (* "l" ^ (string_of_int (1 + l)) in *)
      "@" ^ lstr
  | FunV (env, x, exp) ->
      let envs = string_of_env env in
      let s = string_of_exp exp in
      "(" ^ envs ^ ")[fun " ^ x ^ " -> " ^ s ^ "]"
  | RecV (env, x, y, exp) ->
      let envs = string_of_env env in
      let s = string_of_exp exp in
      "(" ^ envs ^ ")[rec " ^ x ^ " = fun " ^ y ^ " -> " ^ s ^ "]"

and string_of_env = function
    [] -> ""
  | (id, v)::[] ->
      let vs = string_of_value v in
      id ^ " = " ^ vs
  | (id, v)::rest ->
      let vs = string_of_value v in
      string_of_env rest ^ ", " ^ id ^ " = " ^ vs

and string_of_store = function
    [] -> ""
  | (l, v) :: [] ->
      let vs = string_of_value v in
      let ls = string_of_value (LocV l) in
      ls ^ " = " ^ vs
  | (l, v)::rest ->
      let vs = string_of_value v in
      let ls = string_of_value (LocV l) in
      string_of_store rest ^ ", " ^ ls ^ " = " ^ vs

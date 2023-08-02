type id = string

type prim = Plus | Minus | Times | Lt
type exp =
    Int of int
  | Bool of bool
  | Var of id
  | BinOp of prim * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | FunExp of id * exp
  | AppExp of exp * exp
  | LetRecExp of id * id * exp * exp
  | Nil
  | Cons of exp * exp
  | MatchExp of exp * exp * id * id * exp

let string_of_op = function
    Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Lt -> "<"

let rec string_of_exp = function
    Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Var id -> id
  | BinOp (op, exp1, exp2) ->
      let ops = string_of_op op in
      let s1 = string_of_exp exp1 in
      let s2 = string_of_exp exp2 in
      "(" ^ s1 ^ " " ^ ops ^ " " ^ s2 ^ ")"
  | IfExp (exp1, exp2, exp3) ->
      let s1 = string_of_exp exp1 in
      let s2 = string_of_exp exp2 in
      let s3 = string_of_exp exp3 in
      "(if " ^ s1 ^ " then " ^ s2 ^ " else " ^ s3 ^ ")"
  | LetExp (id, exp1, exp2) ->
      let s1 = string_of_exp exp1 in
      let s2 = string_of_exp exp2 in
      "(let " ^ id ^ " = " ^ s1 ^ " in " ^ s2 ^ ")"
  | FunExp (id, exp) ->
      let s = string_of_exp exp in
      "(fun " ^ id ^ " -> " ^ s ^ ")"
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
      "(match " ^ s1 ^ " with [] -> " ^ s2
      ^ " | " ^ x ^ " :: " ^ y ^ " -> " ^ s3 ^ ")" 

let pp_exp exp = print_string (string_of_exp exp)


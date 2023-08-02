open Syntax
open Eval

type judge =
    EVJ of env * exp * cont * value
  | VVJ of value * cont * value
  | PlusJ of int * int * int
  | MinusJ of int * int * int
  | TimesJ of int * int * int
  | LtJ of int * int * bool

type rule = string
type proof =
    Lf
  | Br of rule * judge * (proof list)

let string_of_judge = function
    EVJ (env, exp, cont, value) ->
      let envs = string_of_env env in
      let s = string_of_exp exp in
      let conts = string_of_cont cont in
      let vs = string_of_value value in
      envs  ^ " |- " ^ s ^ " >> " ^ conts ^ " evalto " ^ vs
  | VVJ (v1, cont, v2) ->
      let s1 = string_of_value v1 in
      let conts = string_of_cont cont in
      let s2 = string_of_value v2 in
      s1 ^ " => " ^ conts ^ " evalto " ^ s2
  | PlusJ (i1, i2, i3) ->
      let s1 = string_of_int i1 in
      let s2 = string_of_int i2 in
      let s3 = string_of_int i3 in
      s1 ^ " plus " ^ s2 ^ " is " ^ s3
  | MinusJ (i1, i2, i3) ->
      let s1 = string_of_int i1 in
      let s2 = string_of_int i2 in
      let s3 = string_of_int i3 in
      s1 ^ " minus " ^ s2 ^ " is " ^ s3
  | TimesJ (i1, i2, i3) ->
      let s1 = string_of_int i1 in
      let s2 = string_of_int i2 in
      let s3 = string_of_int i3 in
      s1 ^ " times " ^ s2 ^ " is " ^ s3
  | LtJ (i1, i2, b3) ->
      let s1 = string_of_int i1 in
      let s2 = string_of_int i2 in
      let s3 = string_of_bool b3 in
      s1 ^ " less than " ^ s2 ^ " is " ^ s3

let rec string_of_proof = function
    Lf -> ""
  | Br (rule, judge, children) ->
      let js = string_of_judge judge in
      let ps = List.map (fun c -> string_of_proof c) children in
      let cstr =
        let rec aux ls = (match ls with
                          [] -> ""
                        | s :: rest -> s ^ "; " ^ aux rest)
        in aux ps in
      js ^ " by " ^ rule ^ " {" ^ cstr ^ "}"

let rec prove judge =
  match judge with
    EVJ (env, exp, cont, value) ->
      (match exp with
        Int i -> Br ("E-Int", judge, [prove (VVJ (IntV i, cont, value))])
      | Bool b -> Br ("E-Bool", judge, [prove (VVJ (BoolV b, cont, value))])
      | IfExp (exp1, exp2, exp3) ->
          let c = EVJ (env, exp1, IfE (env, exp2, exp3, cont), value) in
          Br ("E-If", judge, [prove c])
      | BinOp (op, exp1, exp2) ->
          let c = EVJ (env, exp1, OpE (env, op, exp2, cont), value) in
          Br ("E-BinOp", judge, [prove c])
      | Var x ->
          let v1 = List.assoc x env in
          let c = VVJ (v1, cont, value) in
          Br ("E-Var", judge, [prove c])
      | LetExp (x, exp1, exp2) ->
          let newcont = LetE (env, x, exp2, cont) in
          let c = EVJ (env, exp1, newcont, value) in
          Br ("E-Let", judge, [prove c])
      | FunExp (x, exp) ->
          let funv = FunV (env, x, exp) in
          let c = VVJ (funv, cont, value) in
          Br ("E-Fun", judge, [prove c])
      | AppExp (exp1, exp2) ->
          let newcont = AppE (env, exp2, cont) in
          let c = EVJ (env, exp1, newcont, value) in
          Br ("E-App", judge, [prove c])
      | LetRecExp (x, y, exp1, exp2) ->
          let newenv = (x, RecV (env, x, y, exp1)) :: env in
          let c = EVJ (newenv, exp2, cont, value) in
          Br ("E-LetRec", judge, [prove c])
      | Nil ->
          let c = VVJ (NilV, cont, value) in
          Br ("E-Nil", judge, [prove c])
      | Cons (exp1, exp2) ->
          let newcont = ConsE (env, exp2, cont) in
          let c = EVJ (env, exp1, newcont, value) in
          Br ("E-Cons", judge, [prove c])
      | MatchExp (exp1, exp2, x, y, exp3) ->
          let newcont = MatchE (env, exp2, x, y, exp3, cont) in
          let c = EVJ (env, exp1, newcont, value) in
          Br ("E-Match", judge, [prove c])
      | LetCC (x, exp) ->
          let newenv = (x, ContV cont) :: env in
          let c = EVJ (newenv, exp, cont, value) in
          Br ("E-LetCc", judge, [prove c]))
  | VVJ (v1, cont, v2) ->
      (match cont, v1 with
        End, _ -> Br ("C-Ret", judge, [])
      | OpE (env, op, exp, cont), _ -> (* C-EvalR *)
          let c = EVJ (env, exp, OpV (op, v1, cont), v2) in
          Br ("C-EvalR", judge, [prove c])
      | OpV (Plus, IntV i1, cont), IntV i2 -> (* C-Plus *)
          let i3 = i1 + i2 in
          let c1 = PlusJ (i1, i2, i3) in
          let c2 = VVJ (IntV i3, cont, v2) in
          Br ("C-Plus", judge, [prove c1; prove c2])
      | OpV (Minus, IntV i1, cont), IntV i2 -> (* C-Minus *)
          let i3 = i1 - i2 in
          let c1 = MinusJ (i1, i2, i3) in
          let c2 = VVJ (IntV i3, cont, v2) in
          Br ("C-Minus", judge, [prove c1; prove c2])
      | OpV (Mult, IntV i1, cont), IntV i2 -> (* C-Times *)
          let i3 = i1 * i2 in
          let c1 = TimesJ (i1, i2, i3) in
          let c2 = VVJ (IntV i3, cont, v2) in
          Br ("C-Times", judge, [prove c1; prove c2])
      | OpV (Lt, IntV i1, cont), IntV i2 -> (* C-Lt *)
          let i3 = i1 < i2 in
          let c1 = LtJ (i1, i2, i3) in
          let c2 = VVJ (BoolV i3, cont, v2) in
          Br ("C-Lt", judge, [prove c1; prove c2])
      | IfE (env, exp1, exp2, cont), BoolV true -> (* C-IfT *)
          let c = EVJ (env, exp1, cont, v2) in
          Br ("C-IfT", judge, [prove c])
      | IfE (env, exp1, exp2, cont), BoolV false -> (* C-IfF *)
          let c = EVJ (env, exp2, cont, v2) in
          Br ("C-IfF", judge, [prove c])
      | LetE (env, id, exp, cont), value -> (* C-LetBody *)
          let newenv = (id, value) :: env in
          let c = EVJ (newenv, exp, cont, v2) in
          Br ("C-LetBody", judge, [prove c])
      | AppE (env, exp, cont), value -> (* C-EvalArg *)
          let newcont = AppV (value, cont) in
          let c = EVJ (env, exp, newcont, v2) in
          Br ("C-EvalArg", judge, [prove c])
      | AppV (FunV (env, id, exp), cont), value -> (* C-EvalFun *)
          let newenv = (id, value) :: env in
          let c = EVJ (newenv, exp, cont, v2) in
          Br ("C-EvalFun", judge, [prove c])
      | AppV (RecV (env, x, y, exp), cont), value -> (* C-EvalFunR *)
          let newenv = (y, value) :: (x, RecV (env, x, y, exp)) :: env in
          let c = EVJ (newenv, exp, cont, v2) in
          Br ("C-EvalFunR", judge, [prove c])
      | AppV (ContV k1, k2), _ -> (* C-EvalFunC *)
          let c = VVJ (v1, k1, v2) in
          Br ("C-EvalFunC", judge, [prove c])
      | ConsE (env, exp, cont), value -> (* C-EvalConsR *)
          let newcont = ConsV (value, cont) in
          let c = EVJ (env, exp, newcont, v2) in
          Br ("C-EvalConsR", judge, [prove c])
      | ConsV (vl, cont), vr -> (* C-Cons *)
          let c = VVJ (ConsV (vl, vr), cont, v2) in
          Br ("C-Cons", judge, [prove c])
      | MatchE (env, exp1, x, y, exp2, cont), NilV -> (* C-MatchNil *)
          let c = EVJ (env, exp1, cont, v2) in
          Br ("C-MatchNil", judge, [prove c])
      | MatchE (env, exp1, x, y, exp2, cont), ConsV (vl, vr) -> (* C-MatchCons *)
          let newenv = (y, vr) :: (x, vl) :: env in
          let c = EVJ (newenv, exp2, cont, v2) in
          Br ("C-MatchCons", judge, [prove c]))
  | PlusJ (i1, i2, i3) ->
      Br ("B-Plus", judge, [])
  | MinusJ (i1, i2, i3) ->
      Br ("B-Minus", judge, [])
  | TimesJ (i1, i2, i3) ->
      Br ("B-Times", judge, [])
  | LtJ (i1, i2, b3) ->
      Br ("B-Lt", judge, [])

let env = []
let exp =
  LetExp("f",
    FunExp("x",
      FunExp("k1",
        FunExp("k2",
          IfExp(BinOp(Lt, Var "x", Int 0),
            AppExp(Var "k1", Var "x"),
            AppExp(Var "k2", Var "x"))))),
    BinOp(Plus,
      Int 1,
      LetCC("k1",
        BinOp(Plus,
          Int 2,
          LetCC("k2",
            AppExp(AppExp(AppExp(Var "f", Int (-2)), Var "k1"), Var "k2"))))))
let ev = IntV (-1)
let cont = End

let j = EVJ (env, exp, cont, ev)
let pf = prove j
let _ = print_endline (string_of_proof pf)

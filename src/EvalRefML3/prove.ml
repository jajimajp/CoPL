open Syntax
open Eval

type judge =
    EvalJ of store * env * exp * value * store
  | PlusJ of int * int * int
  | MinusJ of int * int * int
  | TimesJ of int * int * int
  | LtJ of int * int * bool

type rule = string
type proof =
    Lf
  | Br of rule * judge * (proof list)

let string_of_judge = function
    EvalJ (s1, env, exp, value, s2) ->
      let s1str =
        (match s1 with [] -> ""
         | _ -> string_of_store s1 ^ " / ") in
      let s2str =
        (match s2 with [] -> ""
         | _ -> " / " ^ string_of_store s2) in
      let envs = string_of_env env in
      let s = string_of_exp exp in
      let vs = string_of_value value in
      s1str ^ envs ^ " |- " ^ s ^ " evalto " ^ vs ^ s2str
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
    EvalJ (s1, env, exp, value, s2) ->
      (match exp with
        Int i -> Br ("E-Int", judge, [])
      | Bool b -> Br ("E-Bool", judge, [])
      | IfExp (exp1, exp2, exp3) ->
          let (BoolV b1, s2) = eval s1 env exp1 in
          let c1 = EvalJ (s1, env, exp1, BoolV b1, s2) in
          let (v, s3, c2, rule) =
            if b1 then
              let (v, s3) = eval s2 env exp2 in
              (v, s3, EvalJ (s2, env, exp2, v, s3), "E-IfT")
            else
              let (v, s3) = eval s2 env exp3 in
              (v, s3, EvalJ (s2, env, exp3, v, s3), "E-IfF")
          in
            Br (rule, judge, [prove c1; prove c2])
      | BinOp (op, exp1, exp2) ->
          let (IntV i1, s2) = eval s1 env exp1 in
          let (IntV i2, s3) = eval s2 env exp2 in
          let c1 = EvalJ (s1, env, exp1, IntV i1, s2) in
          let c2 = EvalJ (s2, env, exp2, IntV i2, s3) in
          let (rule, c3) = (match op with
                      Plus  -> ("E-Plus",  PlusJ  (i1, i2, (i1 + i2)))
                    | Minus -> ("E-Minus", MinusJ (i1, i2, (i1 - i2)))
                    | Mult  -> ("E-Mult", TimesJ  (i1, i2, (i1 * i2)))
                    | Lt    -> ("E-Lt",    LtJ    (i1, i2, (i1 < i2)))) in
          Br (rule, judge, [prove c1; prove c2; prove c3])
      | Var x -> Br ("E-Var", judge, [])
      | LetExp (x, exp1, exp2) ->
          let (v1, s2) = eval s1 env exp1 in
          let env2 = (x, v1) :: env in
          let (v2, s3) = eval s2 env2 exp2 in
          let c1 = EvalJ (s1, env, exp1, v1, s2) in
          let c2 = EvalJ (s2, env2, exp2, v2, s3) in
          Br ("E-Let", judge, [prove c1; prove c2])
      | FunExp (x, exp) -> Br ("E-Fun", judge, [])
      | AppExp (exp1, exp2) ->
          let (v1, s2) = eval s1 env exp1 in
          let c1 = EvalJ (s1, env, exp1, v1, s2) in
          let (v2, s3) = eval s2 env exp2 in
          let c2 = EvalJ (s2, env, exp2, v2, s3) in
          let (rule, c3) = (match v1 with
                    FunV (env2, x, exp0) ->
                      let newenv = (x, v2) :: env2 in
                      let (v3, s4) = eval s3 newenv exp0 in
                      ("E-App", EvalJ (s3, newenv, exp0, value, s4))
                  | RecV (env2, x, y, exp0) ->
                      let newenv = (y, v2) :: (x, RecV (env2, x, y, exp0)) :: env2 in
                      let (v3, s4) = eval s3 newenv exp0 in
                      ("E-AppRec", EvalJ (s3, newenv, exp0, value, s4))) in
          Br (rule, judge, [prove c1; prove c2; prove c3])
      | LetRecExp (x, y, exp1, exp2) ->
          let newenv = (x, RecV (env, x, y, exp1)) :: env in
          let (v, s2) = eval s1 newenv exp2 in
          let c = EvalJ (s1, newenv, exp2, v, s2) in
          Br ("E-LetRec", judge, [prove c])
      | RefExp exp ->
          let (v, s2) = eval s1 env exp in
          let c = EvalJ (s1, env, exp, v, s2) in
          Br ("E-Ref", judge, [prove c])
      | Deref exp ->
          let (v, s2) = eval s1 env exp in
          let c = EvalJ (s1, env, exp, v, s2) in
          Br ("E-Deref", judge, [prove c])
      | Assign (exp1, exp2) ->
          let (LocV l, s2) = eval s1 env exp1 in
          let (v, s3) = eval s2 env exp2 in
          let s4 = assign s3 l v in
          let c1 = EvalJ (s1, env, exp1, LocV l, s2) in
          let c2 = EvalJ (s2, env, exp2, v, s3) in
          Br ("E-Assign", judge, [prove c1; prove c2]))
  | PlusJ (i1, i2, i3) ->
      Br ("B-Plus", judge, [])
  | MinusJ (i1, i2, i3) ->
      Br ("B-Minus", judge, [])
  | TimesJ (i1, i2, i3) ->
      Br ("B-Mult", judge, [])
  | LtJ (i1, i2, b3) ->
      Br ("B-Lt", judge, [])

let s1 = []
let env = []
let exp =
  LetRecExp("do", "f",
    FunExp("i",
      IfExp(BinOp(Lt, Var "i", Int 1),
        Int 0,
        LetExp("x",
          AppExp(Var "f", Var "i"),
          AppExp(AppExp(Var "do", Var "f"), BinOp(Minus, Var "i", Int 1))))),
    LetExp("x",
      RefExp(Int 0),
      LetExp("sum",
        FunExp("i",
          Assign(Var "x",
            BinOp(Plus, Deref(Var "x"), Var "i"))),
        LetExp("y",
          AppExp(AppExp(Var "do", Var "sum"), Int 3),
          Deref(Var "x")))))
let ev = IntV 6
let s2 = [(0, IntV 6)]
      
let pf = prove (EvalJ (s1, env, exp, ev, s2))

let _ = print_endline (string_of_proof pf)

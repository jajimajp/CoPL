type op = PlusOp | MinusOp | TimesOp | LessThanOp
type v =
  | IntV of int
  | BoolV of bool
  | FunV of env * e * e
  | RecV of env * e * e * e
and e =
  | Int of int
  | Bool of bool
  | Var of string
  | Op of op * e * e
  | If of e * e * e
  | Let of e * e * e
  | Fun of e * e
  | App of e * e
  | LetRec of e * e * e * e
and env = (string * v) list
(* env は先頭が表記時の右側にくる *)

type judge =
  | Evalto of env * e * v
  | Plus of v * v * v
  | Minus of v * v * v
  | Times of v * v * v
  | LessThan of v * v * v

type tree =
  | Nil
  | Node of string * judge * (tree list)
(* nodeに判断規則名が必要 *)

(* 何度もevalすることは一旦気にしない *)

exception Never
let rec findvar (env, name) =
  match env with
  | [] -> raise Never
  | (n, v)::rest -> if name = n
                    then v else findvar (rest, name)
let rec eval (env, e) =
  match e with
  | Int i -> IntV i
  | Bool b -> BoolV b
  | Var s -> findvar (env, s)
  | Op (op, e1, e2) ->
      let e1val = eval (env, e1) in
      let e2val = eval (env, e2) in
        begin match op with
        | PlusOp ->
            begin match (e1val, e2val) with
            | IntV i1, IntV i2 -> IntV (i1 + i2)
            end
        | MinusOp ->
            begin match (e1val, e2val) with
            | IntV i1, IntV i2 -> IntV (i1 - i2)
            end
        | TimesOp ->
            begin match (e1val, e2val) with
            | IntV i1, IntV i2 -> IntV (i1 * i2)
            end
        | LessThanOp ->
            begin match (e1val, e2val) with
            | IntV i1, IntV i2 ->
                if i1 < i2 then BoolV true
                else BoolV false
            end
        | _ -> IntV (-1)
        end
  | If (cond, e1, e2) ->
      let BoolV condv = eval (env, cond) in
      if condv then eval (env, e1)
      else eval (env, e2)
  | Let (e1, e2, e3) ->
      let Var varname = e1 in
      let newenv = (varname, eval (env, e2))::env in
        eval (newenv, e2)
  | Fun (e1, e2) -> FunV (env, e1, e2)
  | App (e1, e2) ->
      begin match eval (env, e1) with
      | FunV (env2, Var x, fune) ->
          let newenv = (x, eval (env, e2))::env2 in
          eval (newenv, fune)
      | RecV (env2, Var x, Var y, e0) ->
          let newenv = (y, eval (env, e2))::(x, eval (env, e1))::env2 in
          eval (newenv, e0)
      end
  | LetRec (e1, e2, e3, e4) -> IntV (-1)

(* returns tree *)
let rec solve j =
  match j with
  | Evalto (env, e, v) ->
      begin match e with
      | Int _ -> Node ("E-Int", j, [])
      | Bool _ -> Node ("E-Bool", j, [])
      | Var s -> let (name, _)::rest = env in
          if name = s then Node ("E-Var1", j, [])
          else let child = solve (Evalto (rest, e, v)) in
            Node ("E-Var2", j, [child])
      | Op (op, e1, e2) ->
          begin match op with
          | PlusOp ->
              Node ("E-Plus", j, [
                solve (Evalto (env, e1, eval (env, e1)));
                solve (Evalto (env, e2, eval (env, e2)));
                solve (Plus (eval (env, e1), eval (env, e2), v))
              ])
          | MinusOp ->
              Node ("E-Minus", j, [
                solve (Evalto (env, e1, eval (env, e1)));
                solve (Evalto (env, e2, eval (env, e2)));
                solve (Minus (eval (env, e1), eval (env, e2), v))
              ])
          | TimesOp ->
              Node ("E-Times", j, [
                solve (Evalto (env, e1, eval (env, e1)));
                solve (Evalto (env, e2, eval (env, e2)));
                solve (Times (eval (env, e1), eval (env, e2), v))
              ])
          | LessThanOp ->
              let evalto = eval (env, e) in
              Node ("E-Lt", j, [
                solve (Evalto (env, e1, eval (env, e1)));
                solve (Evalto (env, e2, eval (env, e2)));
                solve (LessThan (eval (env, e1), eval (env, e2), v))
              ])
          end
      | If (e1, e2, e3) ->
          let BoolV e1val = eval (env, e1) in
          if e1val then
            Node ("E-IfT", j, [
              solve (Evalto (env, e1, BoolV true));
              solve (Evalto (env, e2, v))
            ])
          else
            Node ("E-IfF", j, [
              solve (Evalto (env, e1, BoolV false));
              solve (Evalto (env, e3, v))
            ])
      | Fun (e1, e2) ->
          Node ("E-Fun", j, [])
      | Let (x, e1, e2) ->
          let Var varname = x in
          let e1val = eval (env, e1) in
          Node ("E-Let", j, [
            solve (Evalto (env, e1, e1val));
            solve (Evalto ((varname, e1val)::env, e2, v));
          ])
      | LetRec (x, y, e1, e2) ->
          let Var xname = x in
          let Var yname = y in
          let newenv = (xname, RecV(
            env, x, y, e1
          ))::env in
          Node ("E-LetRec", j, [
            solve(Evalto (newenv, e2, v))
          ])
      | App (e1, e2) ->
          let evalto = eval (env, e1) in 
          begin match evalto with
          | FunV (env2, Var x, fune) ->
              Node ("E-App", j, [
                solve (Evalto (env, e1, evalto));
                solve (Evalto (env, e2, eval (env, e2)));
                let newenv = (x, eval (env, e2))::env2 in
                  solve (Evalto (newenv, fune, v))
              ])
          | RecV (env2, Var x, Var y, e0) ->
              let e1val = evalto in
              let e2val = eval (env, e2) in
              let newenv = (y, e2val)::(x, e1val)::env2 in
              Node ("E-AppRec", j, [
                solve(Evalto(env, e1, e1val));
                solve(Evalto(env, e2, e2val));
                solve(Evalto(newenv, e0, v))
              ])
          end
      end
  | Plus (e1, e2, v) -> Node ("B-Plus", j, [])
  | Minus (e1, e2, v) -> Node ("B-Minus", j, [])
  | Times (e1, e2, v) -> Node ("B-Times", j, [])
  | LessThan (e1, e2, v) -> Node ("B-Lt", j, [])


(* セミコロン区切りでつなげる *)
let rec accum_str f lst =
  match lst with
  | [] -> ""
  | hd::[] -> f hd
  | hd::tl -> f hd ^ "; " ^ accum_str f tl

let rec e_to_string e =
  match e with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Var s -> s
  | Op (op, e1, e2) ->
      begin match op with
      | PlusOp ->
          "(" ^ e_to_string e1 ^ " + " ^ e_to_string e2 ^ ")"
      | MinusOp ->
          "(" ^ e_to_string e1 ^ " - " ^ e_to_string e2 ^ ")"
      | TimesOp ->
          "(" ^ e_to_string e1 ^ " * " ^ e_to_string e2 ^ ")"
      | LessThanOp ->
          "(" ^ e_to_string e1 ^ " < " ^ e_to_string e2 ^ ")"
      end
  | If (e1, e2, e3) ->
      "(if " ^ e_to_string e1 ^ " then " ^ e_to_string e2 ^ " else " ^ e_to_string e3 ^ ")"
  | Fun (e1, e2) ->
      "(fun " ^ e_to_string e1 ^ " -> " ^ e_to_string e2 ^ ")"
  | Let (x, e1, e2) ->
      let Var varname = x in
      "(let " ^ varname ^ " = " ^ e_to_string e1 ^ " in " ^ e_to_string e2 ^ ")"
  | LetRec (Var x, Var y, e1, e2) ->
      "(let rec " ^ x ^ " = fun " ^ y ^ " -> "
        ^ e_to_string e1 ^ " in " ^ e_to_string e2 ^ ")"
  | App (e1, e2) ->
      "(" ^ e_to_string e1 ^ " " ^ e_to_string e2 ^ ")"
let reverse lst =
  let rec rev_rec lst acc =
    match lst with
    | [] -> acc
    | hd::tl -> rev_rec tl (hd::acc)
  in rev_rec lst []
let rec v_to_string v =
  match v with
  | IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | FunV (env, e1, e2) ->
      "(" ^ env_to_string env ^ ")[fun "
        ^ e_to_string e1 ^ " -> "
        ^ e_to_string e2 ^ "]"
  | RecV (env, x, y, e0) ->
      "(" ^ env_to_string env ^ ")[rec "
        ^ e_to_string x ^ " = fun "
        ^ e_to_string y ^ " -> "
        ^ e_to_string e0 ^ "]"
and env_to_string env =
  let rec env_rec env =
    match env with
    | [] -> ""
    | (n, v)::[] -> n ^ " = " ^ v_to_string v
    | (n, v)::rest -> n ^ " = " ^ v_to_string v ^ ", " ^ env_rec rest
  in env_rec (reverse env)
let judge_to_string judge =
  match judge with
  | Evalto (env, e, v) ->
      let env_str = env_to_string env ^ " |- " in
      let v_str = v_to_string v in
          env_str ^ e_to_string e ^ " evalto " ^ v_str
  | Plus (v1, v2, v3) ->
      let v1str = v_to_string v1 in
      let v2str = v_to_string v2 in
      let v3str = v_to_string v3 in
      v1str ^ " plus " ^ v2str ^ " is " ^ v3str
  | Minus (v1, v2, v3) ->
      let v1str = v_to_string v1 in
      let v2str = v_to_string v2 in
      let v3str = v_to_string v3 in
      v1str ^ " minus " ^ v2str ^ " is " ^ v3str
  | Times (v1, v2, v3) ->
      let v1str = v_to_string v1 in
      let v2str = v_to_string v2 in
      let v3str = v_to_string v3 in
      v1str ^ " times " ^ v2str ^ " is " ^ v3str
  | LessThan (v1, v2, v3) ->
      let v1str = v_to_string v1 in
      let v2str = v_to_string v2 in
      let v3str = v_to_string v3 in
      v1str ^ " less than " ^ v2str ^ " is " ^ v3str
let rec tree_to_string tree =
  match tree with
  | Nil -> ""
  | Node (name, j, children) ->
      judge_to_string j ^ " by " ^ name ^ " { " ^ accum_str tree_to_string children ^ " }"


let ev = []
let exp =
  Let(Var "fact",
    Fun(Var "self",
      Fun(Var "n",
        If(Op(LessThanOp, Var "n", Int 2),
          Int 1,
          Op(TimesOp,
            Var "n",
            App(
              App(Var "self", Var "self"),
              Op(MinusOp, Var "n", Int 1)))))),
    App(
      App (Var "fact", Var "fact"),
      Int 3))

let evalto = IntV 6
let j_tosolve = Evalto (ev, exp, evalto)

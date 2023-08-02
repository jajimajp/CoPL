type op = PlusOp | MinusOp | TimesOp | LessThanOp
type types = TBool | TInt | TFun of types * types | TList of types | Any
type env = (string * types) list
type exp =
    Int of int
  | Bool of bool
  | Var of string
  | Op of op * exp * exp
  | If of exp * exp * exp
  | Let of string * exp * exp (* 変数名 = e1 in e2 *)
  | Fun of string * exp (* 引数名 -> e *)
  | App of exp * exp
  | LetRec of string * string * exp * exp (* let rec 関数名 = fun 引数名 -> ... *)
  | Nil
  | Cons of exp * exp
  | Match of exp * exp * string * string * exp (* e1, e2, x, y, e3 *)
type judge = env * exp * types (* in <env>, the type of <e> is <types> *)

(* 導出木 *)
type tree =
    Lf
  | Br of string * judge * (tree list)

let rec find_type env name =
  let (n, t)::rest = env in
  if n = name then t else find_type rest name

let rec eval_type env e =
  match e with
    Int _ -> TInt
  | Bool _ -> TBool
  | Var s -> find_type env s
  | Op (op, e1, e2) ->
      begin
        match op with
          PlusOp | MinusOp | TimesOp -> TInt
        | LessThanOp -> TBool
      end
  | If (e1, e2, e3) -> eval_type env e2
  | Let (x, e1, e2) ->
      let xtype = eval_type env e1 in
      let env = (x, xtype)::env in
      eval_type env e2
  | Fun (x, e) ->
      TFun(Any, Any)
  | App (e1, e2) ->
      let TFun (_, ret) = eval_type env e1 in ret
  | LetRec (x, y, e1, e2) ->
      let env = (y, Any)::(x, Any)::env in
      eval_type env e2
  | Nil -> TList Any
  | Cons (e1, e2) ->
      let t1 = eval_type env e1 in
      TList t1
  | Match (e1, e2, x, y, e3) ->
      eval_type env e2

let rec solve j =
  let (env, exp, t) = j in
  match exp with
    Int _ -> Br ("T-Int", j, [])
  | Bool _ -> Br ("T-Bool", j, [])
  | Var _ -> Br ("T-Var", j, [])
  | Op (op, e1, e2) ->
      let br_left = solve (env, e1, TInt) in
      let br_right = solve (env, e2, TInt) in
      begin
        match op with
          PlusOp -> Br ("T-Plus", j, [br_left; br_right])
        | MinusOp -> Br ("T-Minus", j, [br_left; br_right])
        | TimesOp -> Br ("T-Times", j, [br_left; br_right])
        | LessThanOp -> Br ("T-Lt", j, [br_left; br_right])
      end
  | If (e1, e2, e3) ->
      let c1 = solve (env, e1, TBool) in
      let c2 = solve (env, e2, t) in
      let c3 = solve (env, e3, t) in
      Br ("T-If", j, [c1; c2; c3])
  | Let (x, e1, e2) ->
      let t1 = eval_type env e1 in
      let c1 = solve (env, e1, t1) in
      let c2 = solve ((x, t1)::env, e2, t) in
      Br ("T-Let", j, [c1; c2])
  | Fun (x, e) ->
      let TFun (t1, t2) = t in
      let env = (x, t1)::env in
      Br ("T-Fun", j, [solve (env, e, t2)])
  | App (e1, e2) ->
      let t1 = eval_type env e1 in
      let c1 = solve (env, e1, t1) in
      match t1 with
        TFun (t1, _) ->
      let c2 = solve (env, e2, 
  | LetRec (x, y, e1, e2) -> failwith "Not implemented"
  | Nil -> Br ("T-Nil", j, [])
  | Cons (e1, e2) ->
      let TList t = t in
      let c1 = solve (env, e1, t) in
      let c2 = solve (env, e2, TList t) in
      Br ("T-Cons", j, [c1; c2])
  | Match (e1, e2, x, y, e3) ->
      failwith "Not implemented"

(* 文字列を繋げて括弧でくくる *)
let cat l =
  let rec self l =
    begin match l with [] -> "" | hd::tl -> hd ^ self tl end
  in "(" ^ self l ^ ")"

let rec string_of_exp e =
  match e with
    Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Var s -> s
  | Op (op, e1, e2) ->
      let e1str = string_of_exp e1 in
      let e2str = string_of_exp e2 in
      let opstr = begin
        match op with
          PlusOp -> " + "
        | MinusOp -> " - "
        | TimesOp -> " * "
        | LessThanOp -> " < "
      end in
      "(" ^ e1str ^ opstr ^ e2str ^ ")"
  | If (e1, e2, e3) ->
      let e1s = string_of_exp e1 in
      let e2s = string_of_exp e2 in
      let e3s = string_of_exp e3 in
      cat ["if ";e1s;" then ";e2s;" else ";e3s]
  | Let (x, e1, e2) ->
      let e1s = string_of_exp e1 in
      let e2s = string_of_exp e2 in
      cat ["let ";x;" = ";e1s;" in ";e2s]
  | Fun (x, e) ->
      let es = string_of_exp e in
      cat ["fun ";x;" -> ";es]
  | App (e1, e2) ->
      let e1s = string_of_exp e1 in
      let e2s = string_of_exp e2 in
      cat [e1s;" ";e2s]
  | LetRec (x, y, e1, e2) ->
      let e1s = string_of_exp e1 in
      let e2s = string_of_exp e2 in
      cat ["let rec ";x;" = fun ";y;" -> ";e1s;" in ";e2s] 
  | Nil -> "[]"
  | Cons (e1, e2) ->
      let e1s = string_of_exp e1 in
      let e2s = string_of_exp e2 in
      cat [e1s;" :: ";e2s]
  | Match (e1, e2, x, y, e3) ->
      let e1s = string_of_exp e1 in
      let e2s = string_of_exp e2 in
      let e3s = string_of_exp e3 in
      cat ["match ";e1s;" with [] -> ";e2s;" | ";
           x;" :: ";y;" -> ";e3s]

let rec string_of_types t =
  match t with
    TInt -> "int"
  | TBool -> "bool"
  | TFun (t1, t2) ->
      cat [string_of_types t1;" -> ";string_of_types t2]
  | TList t -> cat [string_of_types t;" list"]

let string_of_env env =
  let rec self env =
    match env with
      [] -> ""
    | (n, t)::rest ->
        n ^ " : " ^ string_of_types t ^
          begin match rest with [] -> "" | _ -> ", " end
  in self (List.rev env)

let string_of_judge j =
  let (env, e, t) = j in
  let envstr = string_of_env env in
  let estr = string_of_exp e in
  let tstr = string_of_types t in
  envstr ^ " |- " ^ estr ^ " : " ^ tstr

let rec string_of_tree t =
  let rec list_children l =
    match l with [] -> ""
    | hd::tl -> (string_of_tree hd) ^
      begin match tl with [] -> ""
      | _ -> "; " ^ list_children tl end in
  let Br (name, j, children) = t in
  string_of_judge j ^ " by " ^ name ^ " { " ^
  (list_children children) ^ " }"

(* 自動インデント *)
(* うまくいかない *)
let rec print_string str index indent =
  let n () = print_char '\n' in
  let rec sp num = (match num with 0 -> () | _ -> (print_char ' '; sp (num - 1))) in
  let len = String.length str in
  if index = len then () else
  begin
    sp indent;
    let rec self str index =
      let len = String.length str in
      if index = len then () else
      try
        let c = str.[index] in
          (
            print_char c;
            match c with
              '{' -> (n (); print_string str (index + 1) (indent + 2))
            | ';' -> (n (); print_string str (index + 1) indent)
            | _ -> self str (index + 1)
          ); ()
      with Invalid_argument s -> ();
    in self str 0 
  end

    


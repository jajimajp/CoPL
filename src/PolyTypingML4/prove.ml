open Syntax
open Typing

type rule = string (* 導出規則名 *)
(* 導出木 *)

type proof =
    Lf
  | Br of rule * (tyenv * exp * types) * (proof list) 

let string_of_tyenv tyenv =
  let rec aux = function (* 環境のstring NOTE: head側を左に書く *)
      [] -> ""
    | (id, tysc)::[] -> id ^ " : " ^ (string_of_tysc tysc)
    | (id, tysc)::rest -> id ^ " : " ^ (string_of_tysc tysc) ^ ", " ^ (aux rest)
  in aux (List.rev tyenv)

let rec string_of_proof = function
    Lf -> ""
  | Br (r, (tyenv, exp, ty), children) ->
      let envs = string_of_tyenv (Environment.to_list tyenv) in
      let es = string_of_exp exp in
      let ts = string_of_ty ty in
      envs ^ " |- " ^ es ^ " : " ^ ts ^ " by " ^ r
      ^ " {"
      ^ (let rec aux = function
           [] -> ""
         | hd::[] -> (string_of_proof hd)
         | hd::tl -> (string_of_proof hd) ^ "; " ^ (aux tl) 
         in aux children)
      ^ "}" 

(* tyenv |- exp : ty を証明する *)
let rec prove tyenv exp ty =
  match exp with
    Int _ -> Br ("T-Int", (tyenv, exp, ty), [])
  | Bool _ -> Br ("T-Bool", (tyenv, exp, ty), [])
  | Var _ -> Br ("T-Var", (tyenv, exp, ty), [])
  | BinOp (op, exp1, exp2) ->
      let ops = string_of_op op in
      let rule = (match op with
                    Plus -> "T-Plus"
                  | Minus -> "T-Minus"
                  | Times -> "T-Mult"
                  | Lt -> "T-Lt") in
      let p1 = prove tyenv exp1 TInt in
      let p2 = prove tyenv exp2 TInt in
      Br (rule, (tyenv, exp, ty), [p1; p2])
  | IfExp (exp1, exp2, exp3) ->
      let p1 = prove tyenv exp1 TBool in
      let p2 = prove tyenv exp2 ty in
      let p3 = prove tyenv exp3 ty in
      Br ("T-If", (tyenv, exp, ty), [p1; p2; p3])
  | LetExp (id, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let tysc1 = closure ty1 tyenv s1 in
      let newenv = Environment.extend id tysc1 tyenv in
      let (s2, ty2) = ty_exp newenv exp2 in
      let eqs = [(ty, ty2)] @ (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in
      let ty1 = subst_type s3 ty1 in
      let p1 = prove tyenv exp1 ty1 in
      let tysc1 = closure ty1 tyenv s1 in
      let newenv = Environment.extend id tysc1 tyenv in (* 確定した型で更新 *)
      let ty2 = subst_type s3 ty2 in
      let p2 = prove newenv exp2 ty2 in
      Br ("T-Let", (tyenv, exp, ty), [p1; p2])
  | FunExp (id, fexp) ->
      let t1 = TVar (fresh_tyvar ()) in
      let t2 = TVar (fresh_tyvar ()) in
      let tfun = TFun (t1, t2) in
      let eqs = [(tfun, ty)] in
      let s = unify eqs in
      let t1 = subst_type s t1 in
      let t2 = subst_type s t2 in
      let newenv = Environment.extend id (tysc_of_ty t1) tyenv in
      let p = prove newenv fexp t2 in
      Br ("T-Abs", (tyenv, exp, ty), [p])
  | AppExp (exp1, exp2) ->
      let (s2, ty2) = ty_exp tyenv exp2 in
      let tfun = TFun (ty2, ty) in
      let (s1, ty1) = ty_exp tyenv exp1 in
      let eqs = [(ty1, tfun)] @ (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in
      let ty1 = subst_type s3 ty1 in
      let ty1 = subst_type s3 ty1 in
      let ty2 = subst_type s3 ty2 in
      let p1 = prove tyenv exp1 ty1 in
      let p2 = prove tyenv exp2 ty2 in
      Br ("T-App", (tyenv, exp, ty), [p1; p2])
  | LetRecExp (x, y, exp1, exp2) ->
      let t1 = TVar (fresh_tyvar ()) in
      let t2 = TVar (fresh_tyvar ()) in
      let tyenv2 = Environment.extend x (tysc_of_ty (TFun (t1, t2))) tyenv in
      let tyenv1 = Environment.extend y (tysc_of_ty t1) tyenv2 in
      let (s1, ty1) = ty_exp tyenv1 exp1 in
      let tyscx = closure (TFun (t1, ty1)) tyenv s1 in
      let tyenv2 = Environment.extend x tyscx tyenv in
      let (s2, ty2) = ty_exp tyenv2 exp2 in
      let eqs = [(ty1, t2); (ty2, ty)] @ (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in
      let t1 = subst_type s3 t1 in
      let t2 = subst_type s3 t2 in
      let ty1 = subst_type s3 ty1 in
      let tyenv2 = Environment.extend x (tysc_of_ty (TFun (t1, t2))) tyenv in  (* update tyenv for p2 *)
      let tyenv1 = Environment.extend y (tysc_of_ty t1) tyenv2 in (* update tyenv for p1 *)
      let p1 = prove tyenv1 exp1 ty1 in
      let tyscx = closure (TFun (t1, ty1)) tyenv s3 in
      let tyenv2 = Environment.extend x tyscx tyenv in
      let p2 = prove tyenv2 exp2 ty in
      Br ("T-LetRec", (tyenv, exp, ty), [p1; p2])
  | Nil ->
      let TList lty = ty in
      Br ("T-Nil", (tyenv, exp, ty), [])
  | Cons (exp1, exp2) ->
      let t1 = TVar (fresh_tyvar ()) in
      let lty = TList t1 in
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let eqs = [(ty1, t1); (ty2, lty); (lty, ty)] @ (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in
      let t1 = subst_type s3 t1 in
      let lty = TList t1 in
      let p1 = prove tyenv exp1 t1 in
      let p2 = prove tyenv exp2 lty in
      Br ("T-Cons", (tyenv, exp, ty), [p1; p2])
  | MatchExp (exp1, exp2, x, y, exp3) ->
      let t = TVar (fresh_tyvar ()) in
      let lty = TList t in
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let newenv = Environment.extend x (tysc_of_ty t) tyenv in
      let newenv = Environment.extend y (tysc_of_ty lty) newenv in
      let (s3, ty3) = ty_exp newenv exp3 in
      let eqs = [(ty1, lty); (ty2, ty); (ty3, ty)] @ (eqs_of_subst s1)
                    @ (eqs_of_subst s2) @ (eqs_of_subst s3) in
      let s4 = unify eqs in
      let t = subst_type s4 t in
      let lty = subst_type s4 lty in
      let p1 = prove tyenv exp1 lty in
      let p2 = prove tyenv exp2 ty in
      let newenv = Environment.extend x (tysc_of_ty t) tyenv in (* update *)
      let newenv = Environment.extend y (tysc_of_ty lty) newenv in
      let p3 = prove newenv exp3 ty in
      Br ("T-Match", (tyenv, exp, ty), [p1; p2; p3])
  | _ -> failwith "Not implemented"

let vars_tysc = function
  TyScheme (vars, ty) ->
    freevar_ty ty

let rec vars_tyenv = function
    [] -> []
  | (id, tysc) :: rest ->
      union (vars_tyenv rest) (vars_tysc tysc)

let rec vars_prove prove = match prove with
  Lf -> []
  | Br (rule, (tyenv, exp, ty), children) ->
    let self = union (vars_tyenv tyenv) (freevar_ty ty) in
    let rec aux ls = (match ls with
                      [] -> []
                    | hd::tl -> union (vars_prove hd)  (aux tl))
    in union (aux children) self

let subst_for_vars vars =
  let rec aux ls idx = match ls with
    [] -> []
  | hd :: tl -> (hd, TVar idx) :: aux tl (idx + 1)
  in aux vars 0

let rec subst_vars subst = function
    [] -> []
  | id :: rest ->
      let rec aux subst = (match subst with
                            [] -> id
                          | (sid, TVar tid) :: rest ->
                              if sid = id then tid
                              else aux rest) in
      aux subst :: subst_vars subst rest

let rec subst_tysc subst = function
  TyScheme (vars, ty) ->
    let vars = subst_vars subst vars in
    let ty = subst_type subst ty in
    TyScheme (vars, ty)

let rec subst_tyenv subst = function
    [] -> []
  | (id, tysc) :: rest -> (id, subst_tysc subst tysc) :: subst_tyenv subst rest

let rec subst_prove subst = function
    Lf -> Lf
  | Br (name, (tyenv, exp, ty), children) ->
      let env = subst_tyenv subst tyenv in
      let ty = subst_type subst ty in
      let children = List.map (subst_prove subst) children in
      Br (name, (env, exp, ty), children)

let tyenv = []
let exp =
  LetExp("f",
    FunExp("x",
      LetExp("g",
        FunExp("y",
          Cons(AppExp(Var "y", Var "x"), Nil)),
        AppExp(Var "g", FunExp("z", Int 4)))),
  MatchExp(AppExp(Var "f", Bool true),
    Cons (Int 3, Nil),
    "x", "y",
    AppExp(Var "f", Var "x")))
let ty = TList TInt 

let pr = prove tyenv exp ty
let vars = vars_prove pr
let s = subst_for_vars vars
let pr = subst_prove s pr

let rec l = function
   [] -> ""
  | hd :: tl -> string_of_int hd ^ " " ^ l tl

let _ = print_endline (string_of_proof pr)

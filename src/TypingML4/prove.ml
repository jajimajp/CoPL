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
    | (id, ty)::[] -> id ^ " : " ^ (string_of_ty ty)
    | (id, ty)::rest -> id ^ " : " ^ (string_of_ty ty) ^ ", " ^ (aux rest)
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
let rec prove tyenv exp ty = match exp with
    Int _ -> Br ("T-Int", (tyenv, exp, ty), [])
  | Bool _ -> Br ("T-Bool", (tyenv, exp, ty), [])
  | Var _ -> Br ("T-Var", (tyenv, exp, ty), [])
  | BinOp (op, exp1, exp2) ->
      let ops = string_of_op op in
      let rule = (match op with
                    Plus -> "T-Plus"
                  | Minus -> "T-Minus"
                  | Times -> "T-Times"
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
      let newenv = Environment.extend id ty1 tyenv in
      let (s2, ty2) = ty_exp newenv exp2 in
      let eqs = [(ty, ty2)] @ (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in
      let ty1 = subst_type s3 ty1 in
      let p1 = prove tyenv exp1 ty1 in
      let newenv = Environment.extend id ty1 tyenv in (* 確定した型で更新 *)
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
      let newenv = Environment.extend id t1 tyenv in
      let p = prove newenv fexp t2 in
      Br ("T-Fun", (tyenv, exp, ty), [p])
  | AppExp (exp1, exp2) ->
      let t1 = TVar (fresh_tyvar ()) in
      let tfun = TFun (t1, ty) in
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let eqs = [(ty1, tfun); (ty2, t1)] @ (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in
      let ty1 = subst_type s3 ty1 in
      let ty2 = subst_type s3 ty2 in
      let p1 = prove tyenv exp1 ty1 in
      let p2 = prove tyenv exp2 ty2 in
      Br ("T-App", (tyenv, exp, ty), [p1; p2])
  | LetRecExp (x, y, exp1, exp2) ->
      let t1 = TVar (fresh_tyvar ()) in
      let t2 = TVar (fresh_tyvar ()) in
      let tyenv2 = Environment.extend x (TFun (t1, t2)) tyenv in  (* tyenv for p2 *)
      let (s2, ty2) = ty_exp tyenv2 exp2 in
      let tyenv1 = Environment.extend y t1 tyenv2 in (* tyenv for p1 *)
      let (s1, ty1) = ty_exp tyenv1 exp1 in
      let eqs = [(ty1, t2); (ty2, ty)] @ (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in
      let t1 = subst_type s3 t1 in
      let t2 = subst_type s3 t2 in
      let ty1 = subst_type s3 ty1 in
      let tyenv2 = Environment.extend x (TFun (t1, t2)) tyenv in  (* update tyenv for p2 *)
      let tyenv1 = Environment.extend y t1 tyenv2 in (* update tyenv for p1 *)
      let p1 = prove tyenv1 exp1 ty1 in
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
      let newenv = Environment.extend x t tyenv in
      let newenv = Environment.extend y lty newenv in
      let (s3, ty3) = ty_exp newenv exp3 in
      let eqs = [(ty1, lty); (ty2, ty); (ty3, ty)] @ (eqs_of_subst s1)
                    @ (eqs_of_subst s2) @ (eqs_of_subst s3) in
      let s4 = unify eqs in
      let t = subst_type s4 t in
      let lty = subst_type s4 lty in
      let p1 = prove tyenv exp1 lty in
      let p2 = prove tyenv exp2 ty in
      let newenv = Environment.extend x t tyenv in (* update *)
      let newenv = Environment.extend y lty newenv in
      let p3 = prove newenv exp3 ty in
      Br ("T-Match", (tyenv, exp, ty), [p1; p2; p3])
  | _ -> failwith "Not implemented"

(* 導出の上で任意の型で良いために残ったTVarを全てintに置き換える *)
let rec subst_int = function
    Lf -> Lf
  | Br (rule, (tyenv, exp, ty), children) ->
      let rec for_ty ty = (match ty with
                            TVar _ -> TInt
                            | TFun (a, b) -> TFun (for_ty a, for_ty b)
                            | TList a -> TList (for_ty a)
                            | _ -> ty) in
      let rec for_env ev = (match ev with
          [] -> []
        | (id, ty)::tl -> (id, (for_ty ty))::(for_env tl)) in
      let rec for_children c = (match c with [] -> [] | hd::tl -> (subst_int hd)::(for_children tl)) in
      Br (rule, ((for_env tyenv), exp, (for_ty ty)), (for_children children))

let tyenv = []
let exp =
  LetRecExp("map", "f", FunExp("l", 
    MatchExp(Var "l",
      Nil,
      "x", "y",
      Cons(
        AppExp(Var "f", Var "x"),
        AppExp(AppExp(Var "map", Var "f"), Var "y")
      )))
  ,
  AppExp(AppExp(Var "map", FunExp("x", BinOp(Lt, Var "x", Int 3))),
    Cons(Int 4, Cons(Int 5, Cons(Int 1, Nil)))
  )

  )
let ty = TList TBool
let pr = prove tyenv exp ty
let pr = subst_int pr
let _ = print_endline (string_of_proof pr)


open Syntax

type tyvar = int

type types =
    TBool
  | TInt
  | TVar of tyvar
  | TFun of types * types
  | TList of types

let rec string_of_ty = function
    TBool -> "bool"
  | TInt -> "int"
  | TVar id ->
      let c = Char.escaped (char_of_int (id + int_of_char 'a')) in
      "'" ^ c
  | TFun (ty1, ty2) ->
      let s1 = string_of_ty ty1 in
      let s2 = string_of_ty ty2 in
      "(" ^ s1 ^ " -> " ^ s2 ^ ")"
  | TList ty ->
      let s = string_of_ty ty in
      "(" ^ s ^ " list)"

let pp_ty ty = print_string (string_of_ty ty)

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
      counter := v + 1; v
    in body

type subst = (tyvar * types) list (* 型代入 *)

let rec subst_type subst ty =
  match subst with
    [] -> ty
  | (tyvar, newty)::tl ->
      let rec aux ty =
        (match ty with
           TBool | TInt -> ty
         | TVar id -> if tyvar = id then newty
                      else ty
         | TFun (ty1, ty2) ->
             let ty1 = aux ty1 in 
             let ty2 = aux ty2 in 
             TFun (ty1, ty2)
         | TList lty -> TList (aux lty))
      in subst_type tl (aux ty)

let rec eqs_of_subst = function
    [] -> []
  | (id, ty)::rest -> (TVar id, ty)::(eqs_of_subst rest)

let rec subst_eqs subst = function
    [] -> []
  | (ty1, ty2)::rest ->
      let fn = subst_type subst in
      (fn ty1, fn ty2)::(subst_eqs subst rest)

(* 型の等式制約 (types * types) list を解いて型代入 subst を返す *)
let rec unify = function
    [] -> []
  | (ty1, ty2)::rest ->
      if ty1 = ty2 then unify rest
      else
       (match ty1, ty2 with
           TFun (t11, t12), TFun (t21, t22) ->
             let newconstr = (rest @ [(t11, t21); (t12, t22)]) in
               unify newconstr
         | TList lt1, TList lt2 -> unify ((lt1, lt2) :: rest)
         | TVar id, _ -> (* TODO: FTV 確認によるエラー表示 *)
             let rest = subst_eqs [(id, ty2)] rest in
             (id, ty2) :: (unify rest)
         | _, TVar id -> (* TODO: FTV 確認によるエラー表示 *)
             let rest = subst_eqs [(id, ty1)] rest in
             (id, ty1) :: (unify rest)
         | TList lt1, TList lt2 -> unify ((lt1, lt2) :: rest)
         | _, _ -> failwith "Error at unify: cannot solve")

type tyenv = types Environment.env

type judge = (Eval.exval Environment.env) * exp * types

(* (制約, 返値型) を返す *)
let ty_prim op ty1 ty2 =
  match op with
    Plus -> ([(ty1, TInt); (ty2, TInt)], TInt)
  | Minus -> ([(ty1, TInt); (ty2, TInt)], TInt)
  | Times -> ([(ty1, TInt); (ty2, TInt)], TInt)
  | Lt -> ([(ty1, TInt); (ty2, TInt)], TBool)

let rec ty_exp tyenv = function
  Int i -> ([], TInt)
  | Bool b -> ([], TBool)
  | Var id -> ([], Environment.assoc id tyenv)
  | BinOp (op, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (eqs3, ty) = ty_prim op ty1 ty2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
      let s3 = unify eqs in (s3, subst_type s3 ty)
  | IfExp (exp1, exp2, exp3) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (s3, ty3) = ty_exp tyenv exp3 in
      let eqs = [(ty1, TBool); (ty2, ty3)] @ (eqs_of_subst s1)
          @ (eqs_of_subst s2) @ (eqs_of_subst s3) in
      let s4 = unify eqs in (s4, subst_type s4 ty2)
  | LetExp (id, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let tyenv = Environment.extend id ty1 tyenv in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in (s3, subst_type s3 ty2)
  | FunExp (id, exp) ->
      let domty = TVar (fresh_tyvar ()) in
      let s, ranty =
        ty_exp (Environment.extend id domty tyenv) exp in
        (s, TFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let targ = TVar (fresh_tyvar ()) in
      let tret = TVar (fresh_tyvar ()) in
      let tfn = TFun (targ, tret) in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let eqs = [(ty1, tfn); (ty2, targ)] @ (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in (s3, subst_type s3 tret)
  | LetRecExp (f, x, exp1, exp2) -> (* let rec f = fun x -> exp1 in exp2 *)
      let t1 = TVar (fresh_tyvar ()) in
      let t2 = TVar (fresh_tyvar ()) in
      let fty = TFun (t1, t2) in
      let tyenv = Environment.extend f fty tyenv in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let tyenv = Environment.extend x t1 tyenv in
      let (s1, ty1) = ty_exp tyenv exp1 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in (s3, subst_type s3 ty2)
  | Nil ->
      let t = TVar (fresh_tyvar ()) in
      ([], TList t)
  | Cons (exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let t = TVar (fresh_tyvar ()) in
      let lty = TList t in
      let eqs = [(ty1, t); (ty2, lty);] @ (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in (s3, subst_type s3 ty2)
  | MatchExp (exp1, exp2, x, y, exp3) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let t1 = TVar (fresh_tyvar ()) in
      let lty = TList t1 in
      let tyenv = Environment.extend x t1 tyenv in
      let tyenv = Environment.extend y lty tyenv in
      let (s3, ty3) = ty_exp tyenv exp3 in
      let eqs = [(ty1, lty); (ty2, ty3)]
                 @ (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) in
      let s4 = unify eqs in (s4, subst_type s4 ty2)
  | _ -> failwith "Not implemented"

(*
let tyenv = Environment.empty
let matchexp =
  LetRecExp("len", "l",
    MatchExp(Var "l",
      Int 0,
      "hd", "tl", BinOp(Plus,
        Int 1,
        AppExp (Var "len", Var "tl")
      )
    ),
    Var "len"
  )

let e97 =
  LetExp("s", FunExp("f", FunExp("g", FunExp("x", AppExp(AppExp(Var "f", Var "x"), AppExp(Var "g", Var "x"))
  ))),
  LetExp("k1", FunExp("x", FunExp("y", Var "x")),
  LetExp("k2", FunExp("x", FunExp("y", Var "x")),
  AppExp(AppExp(Var "s", Var "k1"), Var "k2")
  )))
let (_, ty) = ty_exp tyenv e97
let _ = pp_ty ty
*)


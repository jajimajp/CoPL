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

type tysc = TyScheme of tyvar list * types

let tysc_of_ty ty = TyScheme ([], ty)

let string_of_tysc = function
  TyScheme (vars, ty) ->
    (match vars with
      [] -> "" (* 空の場合はドット区切りを含めない *)
    | _ ->
      let rec aux vars = (match vars with
        [] -> ""
      | tyvar::tl -> string_of_ty (TVar tyvar) ^ " " ^ (aux tl))
      in aux vars ^ " . ") ^ string_of_ty ty

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

let rec string_of_eqs = function
    [] -> ""
  | (ty1, ty2)::rest ->
      "(" ^ string_of_ty ty1 ^ "=" ^ string_of_ty ty2 ^ "), " ^ string_of_eqs rest

let pp_eqs eqs = print_endline (string_of_eqs eqs)

let rec subst_eqs subst = function
    [] -> []
  | (ty1, ty2)::rest ->
      let fn = subst_type subst in
      (fn ty1, fn ty2)::(subst_eqs subst rest)

(* 型の等式制約 (types * types) list を解いて型代入 subst を返す *)
let rec unify eqs =
  match eqs with
    [] -> []
  | (ty1, ty2)::rest ->
      if ty1 = ty2 then unify rest
      else
       (match ty1, ty2 with
           TFun (t11, t12), TFun (t21, t22) ->
             let newconstr = (rest @ [(t11, t21); (t12, t22)]) in
               unify newconstr
         | TList lt1, TList lt2 -> unify ((lt1, lt2) :: rest)
         | TVar i1, TVar i2 -> (* HACK: 若い方に合わせてみる *)
             if i1 < i2 then 
               let rest = subst_eqs [(i2, TVar i1)] rest in
               (i2, TVar i1) :: (unify rest)
             else 
               let rest = subst_eqs [(i1, TVar i2)] rest in
               (i1, TVar i2) :: (unify rest)
         | TVar id, _ -> (* TODO: FTV 確認によるエラー表示 *)
             let rest = subst_eqs [(id, ty2)] rest in
             (id, ty2) :: (unify rest)
         | _, TVar id -> (* TODO: FTV 確認によるエラー表示 *)
             let rest = subst_eqs [(id, ty1)] rest in
             (id, ty1) :: (unify rest)
         | TList lt1, TList lt2 -> unify ((lt1, lt2) :: rest)
         | _, _ -> failwith "Error at unify: cannot solve")

type tyenv = tysc Environment.env

type judge = (Eval.exval Environment.env) * exp * types

let rec insert x = function
    [] -> [x]
  | y::rest -> if x = y then y::rest else y :: insert x rest

let union xs ys =
  List.fold_left (fun zs x -> insert x zs) ys xs

let rec remove x = function
    [] -> []
  | y :: rest -> if x = y then rest else y :: remove x rest

let diff xs ys =
  List.fold_left (fun zs x -> remove x zs) xs ys

let rec bigunion = function
    [] -> []
  | set1 :: rest -> union set1 (bigunion rest)

let rec freevar_ty = function
    TInt | TBool -> []
  | TVar id -> [id]
  | TFun (ty1, ty2) ->
      union (freevar_ty ty1) (freevar_ty ty2)
  | TList ty -> freevar_ty ty

let freevar_tysc tysc =
  let TyScheme (vars, ty) = tysc in
  let fv = freevar_ty ty in
  diff fv vars

let rec freevar_tyenv tyenv =
  let aux tyenvelm fv =
    let (id, tysc) = tyenvelm in
    union fv (freevar_tysc tysc) in
  List.fold_right aux tyenv []

let closure ty tyenv subst =
  let fv_tyenv' = freevar_tyenv tyenv in
  let fv_tyenv =
    bigunion
      (List.map
        (fun id -> freevar_ty (subst_type subst (TVar id)))
        fv_tyenv') in
  let ids = diff (freevar_ty ty) fv_tyenv in
    TyScheme (ids, ty)

(* (制約, 返値型) を返す *)
let ty_prim op ty1 ty2 =
  match op with
    Plus -> ([(ty1, TInt); (ty2, TInt)], TInt)
  | Minus -> ([(ty1, TInt); (ty2, TInt)], TInt)
  | Times -> ([(ty1, TInt); (ty2, TInt)], TInt)
  | Lt -> ([(ty1, TInt); (ty2, TInt)], TBool)

let rec ty_exp tyenv exp =
  match exp with
  Int i -> ([], TInt)
  | Bool b -> ([], TBool)
  | Var id ->
      let TyScheme (vars, ty) = Environment.assoc id tyenv in
      let s = List.map (fun id -> (id, TVar (fresh_tyvar ()))) vars in
      ([], subst_type s ty)
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
      let tysc1 = closure ty1 tyenv s1 in
      let tyenv = Environment.extend id tysc1 tyenv in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in (s3, subst_type s3 ty2)
  | FunExp (id, exp) ->
      let domty = TVar (fresh_tyvar ()) in
      let s, ranty =
        ty_exp (Environment.extend id (tysc_of_ty domty) tyenv) exp in
        (s, TFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) ->
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (s1, ty1) = ty_exp tyenv exp1 in
      let targ = ty2 in
      let tret = TVar (fresh_tyvar ()) in
      let tfn = TFun (targ, tret) in
      let eqs = [(ty1, tfn); (ty2, targ)] @ (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in (s3, subst_type s3 tret)
  | LetRecExp (f, x, exp1, exp2) -> (* let rec f = fun x -> exp1 in exp2 *)
      let t1 = TVar (fresh_tyvar ()) in
      let t2 = TVar (fresh_tyvar ()) in
      let fty = TFun (t1, t2) in
      let newenv = Environment.extend f (tysc_of_ty fty) tyenv in
      let newenv = Environment.extend x (tysc_of_ty t1) newenv in
      let (s1, ty1) = ty_exp tyenv exp1 in
      let eqs = [(ty1, t2)] @ (eqs_of_subst s1) in
      let s = unify eqs in
      let tysc1 = closure fty tyenv s in
      let tyenv = Environment.extend f tysc1 tyenv in
      let (s2, ty2) = ty_exp tyenv exp2 in
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
      let tyenv = Environment.extend x (tysc_of_ty t1) tyenv in
      let tyenv = Environment.extend y (tysc_of_ty lty) tyenv in
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


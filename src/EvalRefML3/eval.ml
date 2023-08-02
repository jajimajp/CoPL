open Syntax

let rec assign store loc value =
  match store with
    [] -> []
  | (l, v) :: rest ->
      if l = loc then (l, value) :: rest
      else (l, v) :: (assign rest loc value)

(* store, env下でのexpを評価し、(value, newstore) を返す *)
let rec eval store env exp =
  match exp with
    Int i -> (IntV i, store)
  | Bool b -> (BoolV b, store)
  | IfExp (exp1, exp2, exp3) ->
      let (v1, s2) = eval store env exp1 in
      (match v1 with
        BoolV true  -> eval s2 env exp2
      | BoolV false -> eval s2 env exp3
      | _ -> failwith "IfExp got non-bool value")
  | BinOp (op, exp1, exp2) ->
      let (IntV i1, s2) = eval store env exp1 in
      let (IntV i2, s3) = eval s2 env exp2 in
      let v =
        (match op with
          Plus  -> IntV  (i1 + i2)
        | Minus -> IntV  (i1 - i2)
        | Mult  -> IntV  (i1 * i2)
        | Lt    -> BoolV (i1 < i2)) in
      (v, s3)
  | Var x ->
      let v = List.assoc x env in 
        (v, store)
  | LetExp (x, exp1, exp2) ->
      let (v1, s2) = eval store env exp1 in
      let env = (x, v1) :: env in
      eval s2 env exp2
  | FunExp (x, exp) -> (FunV (env, x, exp), store)
  | AppExp (exp1, exp2) -> (* E-App *)
      let (v1, s2) = eval store env exp1 in
      let (v2, s3) = eval s2 env exp2 in
      (match v1 with
        FunV (env2, x, exp0) ->
          eval s3 ((x, v2) :: env2) exp0
      |  RecV (env2, x, y, exp0) ->
          let newenv = (y, v2) :: (x, RecV (env2, x, y, exp0)) :: env2 in
          eval s3 newenv exp0)
  | LetRecExp (x, y, exp1, exp2) -> (* E-LetRec *)
      let newenv = (x, RecV (env, x, y, exp1)) :: env in
      eval store newenv exp2
  | RefExp exp ->
      let (v, s2) = eval store env exp in
      let l = List.length s2 in (* 使用されていないインデックス *)
      (LocV l, (l, v) :: s2)
  | Deref exp ->
      let (LocV l, s2) = eval store env exp in
      let v = List.assoc l s2 in
      (v, s2)
  | Assign (exp1, exp2) ->
      let (LocV l, s2) = eval store env exp1 in
      let (v, s3) = eval s2 env exp2 in
      let s4 = assign s3 l v in
      (v, s4)


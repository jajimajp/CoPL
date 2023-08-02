let tyenv = [("f", TyScheme ([0; 1], TFun(TVar 0, TFun (TVar 1, TVar 0))))]
let exp =
  BinOp(Plus,
    AppExp(AppExp(Var "f", Int 3), Bool true),
    AppExp(AppExp(Var "f", Int 2), Int 4))
let ty = TInt

let env = []
let exp =
  LetExp("s", FunExp("f", FunExp("g", FunExp("x", AppExp(AppExp(Var "f", Var "x"), AppExp(Var "g", Var "x"))
  ))),
  LetExp("k1", FunExp("x", FunExp("y", Var "x")),
  LetExp("k2", FunExp("x", FunExp("y", Var "x")),
  AppExp(AppExp(AppExp(Var "s", Var "k1"), Var "k2"), FunExp("x", BinOp(Plus, Var "x", Int 1)))
  )))
let ty = TFun(TInt,TInt)

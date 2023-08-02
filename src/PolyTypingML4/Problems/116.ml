let tyenv = []
let exp =
  LetExp("s",
    FunExp("f",
      FunExp("g",
        FunExp("x",
          AppExp(AppExp(Var "f", Var "x"), AppExp(Var "g", Var "x"))))),
    LetExp("k",
      FunExp("x",
        FunExp("y",
          Var "x")),
      AppExp(AppExp(Var "s", Var "k"), Var "k")))
let ty = TFun(TVar 0, TVar 0)

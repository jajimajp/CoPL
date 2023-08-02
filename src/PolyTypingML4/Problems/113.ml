let tyenv = []
let exp =
  LetExp("compose",
    FunExp("f",
      FunExp("g",
        FunExp("x",
          AppExp(Var "f", AppExp(Var "g", Var "x"))))),
    LetExp("f",
      FunExp("x",
        IfExp(Var "x",
          Int 3,
          Int 4)),
      LetExp("g",
        FunExp("x",
          BinOp(Lt,
            Var "x",
            Int 4)),
        AppExp(AppExp(AppExp(Var "compose", Var "f"), AppExp(AppExp(Var "compose", Var "g"), Var "f")), Bool true))))
let ty = TInt


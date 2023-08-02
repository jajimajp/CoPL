let tyenv = []
let exp =
  LetExp("twice",
    FunExp("f",
      FunExp("x",
        AppExp(Var "f", AppExp(Var "f", Var "x")))
    ),
    AppExp(AppExp(Var "twice", FunExp("x", BinOp(Plus, Var "x", Int 4))), Int 5)
  )
let ty = TInt

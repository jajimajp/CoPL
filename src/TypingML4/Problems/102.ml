let tyenv = []
let exp =
  LetRecExp("length", "l",
    MatchExp(Var "l",
      Int 0,
      "x", "y",
      BinOp(Plus, Int 1, AppExp(Var "length", Var "y"))
    ),
    Var "length"
  )
let ty = TFun(TList TInt, TInt)

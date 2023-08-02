let tyenv = []
let exp =
  LetRecExp("length", "l",
    MatchExp(Var "l",
      Int 0,
      "x", "y",
      BinOp(Plus, Int 1, AppExp(Var "length", Var "y"))
    ),
    AppExp(Var "length",
      Cons(FunExp("x", Var "x"), Cons(FunExp("y", BinOp(Plus, Var "y", Int 3)), Nil))
    )
  )
let ty = TInt

let tyenv = []
let exp =
  LetExp("f",
    FunExp("x",
      LetExp("g",
        FunExp("y",
          Cons(AppExp(Var "y", Var "x"), Nil)),
        AppExp(Var "g", FunExp("z", Int 4)))),
  MatchExp(AppExp(Var "f", Bool true),
    Cons (Int 3, Nil),
    "x", "y",
    AppExp(Var "f", Var "x")))
let ty = TList TInt 

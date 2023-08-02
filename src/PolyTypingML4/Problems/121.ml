let tyenv = []
let exp =
  LetRecExp("map", "f",
    FunExp("l",
      MatchExp(Var "l",
        Nil,
        "x", "y",
        Cons(AppExp(Var "f", Var "x"), AppExp(AppExp(Var "map", Var "f"), Var "y")))),
    LetExp("f", AppExp(Var "map", FunExp("x", Var "x")),
      LetExp("a", AppExp(Var "f", Cons(Int 3, Nil)),
        AppExp(Var "f", Cons(Bool true, Nil)))))
let ty = TList TBool

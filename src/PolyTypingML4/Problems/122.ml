let tyenv = []
let exp =
  LetExp("f",
    FunExp("x",
      LetExp("g",
        FunExp("y", Cons(Var "x", Nil)),
        IfExp(Bool true,
          AppExp(Var "g", Int 3),
          AppExp(Var "g", Bool false)))),
    MatchExp(AppExp(Var "f", Int 2),
      AppExp(Var "f", Bool true),
      "x", "y",
      Nil))
let ty = TList TBool

let tyenv = []
let exp =
  LetRecExp("map", "f",
    FunExp("l",
      MatchExp(Var "l",
        Nil,
        "x", "y",
        Cons(AppExp(Var "f", Var "x"), AppExp(AppExp(Var "map", Var "f"), Var "y")))),
    AppExp(
      AppExp(Var "map", FunExp("x", BinOp(Lt, Var "x", Int 3))),
      AppExp(
        AppExp(Var "map", FunExp("x", BinOp(Times, Var "x", Int 2))),
        Cons(Int 4, Cons (Int 5, Cons (Int 1, Nil))))))
let ty = TList TBool

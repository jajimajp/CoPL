let tyenv = []
let exp =
  LetExp("k",
    FunExp("x",
      FunExp("y", Var "x")),
    Cons(
      AppExp(AppExp(Var "k", Int 3), Bool true),
      AppExp(AppExp(Var "k", Cons(Int 1, Nil)), Int 3)))
let ty = TList TInt

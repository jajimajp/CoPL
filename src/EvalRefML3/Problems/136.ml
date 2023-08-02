let env = []
let exp =
  LetExp("sm",
    FunExp("f",
      BinOp(Plus,
        AppExp(Var "f", Int 3),
        AppExp(Var "f", Int 4))),
    LetCC("k",
      AppExp(Var "sm", Var "k")))
let ev = IntV 3
let cont = End


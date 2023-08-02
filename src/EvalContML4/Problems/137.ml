let env = []
let exp =
  LetExp("f",
    FunExp("x",
      FunExp("k1",
        FunExp("k2",
          IfExp(BinOp(Lt, Var "x", Int 0),
            AppExp(Var "k1", Var "x"),
            AppExp(Var "k2", Var "x"))))),
    BinOp(Plus,
      Int 1,
      LetCC("k1",
        BinOp(Plus,
          Int 2,
          LetCC("k2",
            AppExp(AppExp(AppExp(Var "f", Int (-2)), Var "k1"), Var "k2"))))))
let ev = IntV (-1)
let cont = End

let s1 = []
let env = []
let exp =
  LetRecExp("do", "f",
    FunExp("i",
      IfExp(BinOp(Lt, Var "i", Int 1),
        Int 0,
        LetExp("x",
          AppExp(Var "f", Var "i"),
          AppExp(AppExp(Var "do", Var "f"), BinOp(Minus, Var "i", Int 1))))),
    LetExp("x",
      RefExp(Int 0),
      LetExp("sum",
        FunExp("i",
          Assign(Var "x",
            BinOp(Plus, Deref(Var "x"), Var "i"))),
        LetExp("y",
          AppExp(AppExp(Var "do", Var "sum"), Int 3),
          Deref(Var "x")))))
let ev = IntV 6
let s2 = [(0, IntV 6)]

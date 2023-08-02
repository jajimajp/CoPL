let s1 = []
let env = []
let exp =
  LetExp("f",
    RefExp(FunExp("x", Var "x")),
    LetExp("fact",
      FunExp("n",
        IfExp(BinOp(Lt, Var "n", Int 1),
          Int 1,
          BinOp(Mult, Var "n", AppExp(Deref(Var "f"), BinOp(Minus, Var "n", Int 1))))),
      LetExp("z",
        Assign(Var "f", Var "fact"),
        AppExp(Var "fact", Int 3))))
let ev = IntV 6
let s2 = [(
  0,
  FunV(
    [("f", LocV 0)],
    "n",
    IfExp(BinOp(Lt, Var "n", Int 1),
      Int 1,
      BinOp(Mult,
        Var "n",
        AppExp(Deref(Var "f"), BinOp(Minus, Var "n", Int 1)))))
)]

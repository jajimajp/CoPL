let s1 = []
let env = []
let exp =
  LetExp("c",
    LetExp("x",
      RefExp(Int 0),
      FunExp("y",
        IfExp(Var "y",
          Assign(Var "x", BinOp(Plus, Deref (Var "x"), Int 1)),
          Deref(Var "x")))),
    LetExp("y", AppExp(Var "c", Bool true),
      LetExp("y", AppExp(Var "c", Bool true),
        AppExp(Var "c", Bool false))))
let ev = IntV 2
let s2 = [(0, IntV 2)]

let s1 = []
let env = []
let exp =
  LetExp("incr",
    FunExp("x",
      Assign(Var "x", BinOp(Plus, Deref (Var "x"), Int 1))),
    LetExp("x", RefExp(Int 0),
      LetExp("z", AppExp(Var "incr", Var "x"),
        Deref (Var "x"))))
let ev = IntV 1
let s2 = [(11, IntV 1)]
      

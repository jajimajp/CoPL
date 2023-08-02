let s1 = []
let env = []
let exp =
  LetExp("newc",
    FunExp("x",
      LetExp("x",
        RefExp(Var "x"),
        FunExp("y",
          IfExp(Var "y",
            Assign(Var "x", BinOp(Plus, Deref (Var "x"), Int 1)),
            Deref (Var "x"))))),
    LetExp("c1",
      AppExp(Var "newc", Int 5),
      LetExp("c2",
        AppExp(Var "newc", Int 4),
        LetExp("y",
          AppExp(Var "c1", Bool true),
          LetExp("y",
            AppExp(Var "c2", Bool true),
            AppExp(Var "c1", Bool false))))))
let ev = IntV 6
let s2 = [(1, IntV 5); (0, IntV 6)]


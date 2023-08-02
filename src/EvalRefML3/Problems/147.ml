let s1 = []
let env = []
let exp =
  LetExp("f",
    FunExp("r1",
      FunExp("r2",
        LetExp("z",
          Assign(Var "r2", Int 3),
          Deref (Var "r1")))),
    LetExp("r",
      RefExp(Int 0),
      AppExp(AppExp(Var "f", Var "r"), Var "r")))
let ev = IntV 3
let s2 = [(0, IntV 3)]

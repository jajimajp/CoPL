let s1 = []
let env = []
let exp =
  LetExp("x",
    RefExp(Int 2),
    LetExp("y",
      RefExp(Int 3),
      LetExp("refx",
        RefExp(Var "x"),
        LetExp("refy",
          RefExp(Var "y"),
          LetExp("z",
            Assign(Deref(Var "refx"), Deref(Deref(Var "refy"))),
            Deref(Var "x"))))))
let ev = IntV 3
let s2 = [(3, LocV 1); (2, LocV 0); (1, IntV 3); (0, IntV 3)]

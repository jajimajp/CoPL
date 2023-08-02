let tyenv = []
let exp =
  LetExp("x",
    Nil,
    LetExp("y",
      Cons(Int 3, Var "x"),
      Cons(Bool true, Var "x")))
let ty = TList TBool

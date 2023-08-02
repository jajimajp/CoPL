let tyenv = []
let exp =
  LetExp("l",
    Cons(FunExp("x", Var "x"), Cons(FunExp("y", Int 2), Cons(FunExp("z", BinOp(Plus, Var "z", Int 3)), Nil)))
    ,
    Int 2
  )
let ty = TInt

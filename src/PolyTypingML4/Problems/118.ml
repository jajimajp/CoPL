let tyenv = []
let exp =
  LetExp("l",
    Cons(FunExp("x", Var "x"), Nil),
    LetExp("l1",
      Cons(FunExp("y", BinOp(Plus, Var "y", Int 1)), Var "l"),
      Cons(
        FunExp("z",
          IfExp(Var "z",
            Bool false,
            Bool true)),
        Var "l"
      )))
let ty = TList (TFun (TBool, TBool))

let tyenv = []
let exp =
  LetRecExp("sum", "f", FunExp("n",
      IfExp(BinOp(Lt, Var "n", Int 1),
        Int 0,
        BinOp(Plus,
          AppExp(Var "f", Var "n"),
          AppExp(AppExp(Var "sum", Var "f"), BinOp(Minus, Var "n", Int 1))
        )
      )
    ),
    AppExp(AppExp(Var "sum", FunExp("x", BinOp(Times, Var "x", Var "x"))), Int 2)
  )
let ty = TInt

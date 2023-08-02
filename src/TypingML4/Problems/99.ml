let tyenv = []
let exp =
  LetRecExp("fact", "n",
    IfExp(BinOp(Lt, Var "n", Int 2),
      Int 1,
      BinOp(Times, Var "n", AppExp(Var "fact", BinOp(Minus, Var "n", Int 1)))
    ),
    AppExp(Var "fact", Int 3)
  )

let ty = TInt

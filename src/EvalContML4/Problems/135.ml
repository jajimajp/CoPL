let env = []
let exp =
  LetRecExp("fact", "n",
    IfExp (BinOp(Lt, Var "n", Int 2), Int 1, BinOp(Mult, Var "n", AppExp(Var "fact", BinOp(Minus, Var "n", Int 1)))),
    BinOp (Plus,
      Int 3,
      LetCC ("k",
        BinOp(Plus,
          BinOp(Plus,
            Int 1,
            AppExp(Var "k", Int 2)),
          AppExp(Var "fact", Int 100)))))
let ev = IntV 5
let cont = End


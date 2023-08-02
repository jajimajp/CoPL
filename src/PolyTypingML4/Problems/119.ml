let tyenv = []
let exp =
  LetRecExp("length", "l",
    MatchExp(Var "l",
      Int 0,
      "x", "y",
      BinOp(Plus, Int 1, AppExp(Var "length", Var "y"))),
    BinOp(Plus,
      AppExp(Var "length",
        Cons(Int 3, Cons (Int 2, Nil))),
      AppExp(Var "length",
        Cons(Cons(Int 1, Nil), Nil))))
let ty = TInt
    

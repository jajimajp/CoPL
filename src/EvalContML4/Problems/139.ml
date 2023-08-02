let env = []
let exp =
  LetRecExp("findneg", "l",
    MatchExp(Var "l",
      Bool false,
      "x", "l",
      IfExp(BinOp(Lt, Var "x", Int 0),
        Bool true,
        AppExp(Var "findneg", Var "l"))),
    AppExp(Var "findneg",
      Cons(Int 1, Cons(Int 2, Cons(Int (-3), Cons(Int 4, Nil))))))
let ev = BoolV true
let cont = []

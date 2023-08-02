let env = []
let exp =
  LetExp("findneg",
    FunExp("l",
      LetCC("k",
        LetRecExp("aux", "l",
          MatchExp(Var "l",
            Bool false,
            "x", "l",
            IfExp(BinOp(Lt, Var "x", Int 0),
              AppExp(Var "k", Bool true),
              AppExp(Var "aux", Var "l"))),
          AppExp(Var "aux", Var "l")))),
    AppExp(Var "findneg",
      Cons(Int 1, Cons(Int 2, Cons(Int (-3), Cons(Int 4, Nil))))))
let ev = BoolV true
let cont = End

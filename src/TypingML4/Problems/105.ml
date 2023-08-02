let tyenv = []
let exp =
  LetRecExp("append", "l1",
    FunExp("l2",
      MatchExp(Var "l1",
        Var "l2",
        "x", "y",
        Cons(Var "x", AppExp(AppExp(Var "append", Var "y"), Var "l2"))
      )
    )
    ,
    AppExp(AppExp(Var "append", Cons (Bool true, Nil)), Cons (Bool false, Nil))
  )
let ty = TList TBool

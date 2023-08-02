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
    Var "append"
  )
let ty = TFun(TList TInt, TFun(TList TInt, TList TInt))

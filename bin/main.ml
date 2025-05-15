open Expr

let sum =
  LetRec
    {
      name = "sum";
      value =
        Fun
          {
            param = "lower";
            body =
              Fun
                {
                  param = "upper";
                  body =
                    Cond
                      {
                        pred = Gt { lhs = Ref "upper"; rhs = Ref "lower" };
                        on_t = Num 0;
                        on_f =
                          Add
                            {
                              lhs = Ref "lower";
                              rhs =
                                Apply
                                  {
                                    f =
                                      Apply
                                        {
                                          f = Ref "sum";
                                          arg =
                                            Add
                                              { lhs = Ref "lower"; rhs = Num 1 };
                                        };
                                    arg = Ref "upper";
                                  };
                            };
                      };
                };
          };
      body = Apply { f = Apply { f = Ref "sum"; arg = Num 1 }; arg = Num 10 };
    }

let num = Expr.Num 1
let bool = Expr.Bool true
let add = Expr.Add { lhs = Expr.Num 1; rhs = Expr.Num 2 }
let gt = Expr.Gt { lhs = Expr.Num 1; rhs = Expr.Num 2 }

let cond =
  Expr.Cond
    {
      pred = Expr.Gt { lhs = Expr.Num 1; rhs = Expr.Num 2 };
      on_t = Expr.Num 3;
      on_f = Expr.Num 4;
    }

let binding =
  Expr.Let
    {
      name = "x";
      value = Expr.Add { lhs = Expr.Num 2; rhs = Expr.Num 3 };
      body = Expr.Add { lhs = Expr.Ref "x"; rhs = Num 1 };
    }

let () =
  let open Run_env in
  let input = sum in
  let value = run input in
  Printf.printf "%s := %s" (pprint input) (Value.pprint value)

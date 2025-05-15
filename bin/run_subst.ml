(* Value handling *****************************************************************************************************)
(**********************************************************************************************************************)

module Value = struct
  type t =
    | Num of int
    | Bool of bool
    | Fun of { param : string; body : Expr.t }

  let pprint value =
    match value with
    | Num i -> string_of_int i
    | Bool b -> string_of_bool b
    | Fun { param; body; _ } ->
        Printf.sprintf "\\%s -> %s" param (Expr.pprint body)
end

(* Substitution *******************************************************************************************************)
(**********************************************************************************************************************)
(* Substitutes all free occurrences of a binding by the specified value.  *)

let subst expr binding value =
  let rec go expr =
    match expr with
    | Expr.Num i -> Expr.Num i
    | Expr.Bool b -> Expr.Bool b
    | Expr.Gt { lhs; rhs } -> Expr.Gt { lhs = go lhs; rhs = go rhs }
    | Expr.Add { lhs; rhs } -> Expr.Add { lhs = go lhs; rhs = go rhs }
    | Expr.Cond { pred; on_t; on_f } ->
        Expr.Cond { pred = go pred; on_t = go on_t; on_f = go on_f }
    | Expr.Let { name; value; body } ->
        Expr.Let
          {
            name;
            value = go value;
            body = (if name = binding then body else go body);
          }
    | Expr.LetRec { name; value; body } ->
        Expr.LetRec
          {
            name;
            value = go value;
            body = (if name = binding then body else go body);
          }
    | Expr.Ref name -> if name == binding then value else Expr.Ref name
    | Expr.Fun { param; body } ->
        Expr.Fun { param; body = (if param = binding then go body else body) }
    | Expr.Apply { f; arg } -> Expr.Apply { f = go f; arg = go arg }
  in
  go expr

let rec run expr =
  match expr with
  | Expr.Num i -> Value.Num i
  | Expr.Bool b -> Value.Bool b
  | Expr.Gt { lhs; rhs } -> run_gt lhs rhs
  | Expr.Add { lhs; rhs } -> run_add lhs rhs
  | Expr.Cond { pred; on_t; on_f } -> run_cond pred on_t on_f
  | Expr.Let { name; value; body } -> run_let name value body
  | Expr.LetRec { name; value; body } -> run_let_rec name value body
  | Expr.Ref name -> failwith "Unbound variable"
  | Expr.Fun { param; body } -> Fun { param; body }
  | Expr.Apply { f; arg } -> run_apply f arg

and eval_and_subst expr binding value =
  let e =
    match run value with
    | Value.Num i -> Expr.Num i
    | Value.Bool b -> Expr.Bool b
    | Value.Fun { param; body } -> Expr.Fun { param; body }
  in
  subst expr binding e

and run_apply f arg =
  match run f with
  | Fun { param; body } -> run (eval_and_subst body param arg)
  | _ -> raise (Failure "type error in function application")

and run_let name value body = run (eval_and_subst body name value)
and run_let_rec name value body = failwith "???"

and run_gt lhs rhs =
  match (run lhs, run rhs) with
  | Value.Num lhs, Value.Num rhs -> Value.Bool (lhs < rhs)
  | _ -> raise (Failure "type error in +")

and run_add lhs rhs =
  match (run lhs, run rhs) with
  | Value.Num lhs, Value.Num rhs -> Value.Num (lhs + rhs)
  | _ -> raise (Failure "type error in +")

and run_cond pred on_t on_f =
  match run pred with
  | Bool true -> run on_t
  | Bool false -> run on_f
  | _ -> raise (Failure "type error in cond")

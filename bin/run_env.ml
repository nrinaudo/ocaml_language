(* Environment handling ***********************************************************************************************)
(**********************************************************************************************************************)

module Env = struct
  type 'a binding = { name : string; mutable value : 'a option }
  type 'a t = 'a binding list

  let find name e =
    let pred = fun b -> b.name = name in
    List.find_opt pred e

  let empty () = []
  let reserve name e = { name; value = None } :: e
  let bind name value e = { name; value = Some value } :: e

  let update name value e =
    match find name e with
    | Some b ->
        b.value <- Some value;
        e
    | None -> failwith "not found"

  let lookup name e =
    match find name e with
    | Some { name = _; value = Some value } -> value
    | _ -> failwith "not found"
end

(* Interpretation result **********************************************************************************************)
(**********************************************************************************************************************)

module Value = struct
  type t =
    | Num of int
    | Bool of bool
    | Fun of { param : string; body : Expr.t; env : t Env.t }

  let pprint value =
    match value with
    | Num i -> string_of_int i
    | Bool b -> string_of_bool b
    | Fun { param; body; _ } ->
        Printf.sprintf "\\%s -> %s" param (Expr.pprint body)
end

(* Interpretation *****************************************************************************************************)
(**********************************************************************************************************************)
open Value

let rec run_env expr e =
  match expr with
  | Expr.Num i -> Num i
  | Expr.Bool b -> Bool b
  | Expr.Gt { lhs; rhs } -> run_gt lhs rhs e
  | Expr.Add { lhs; rhs } -> run_add lhs rhs e
  | Expr.Cond { pred; on_t; on_f } -> run_cond pred on_t on_f e
  | Expr.Let { name; value; body } -> run_let name value body e
  | Expr.LetRec { name; value; body } -> run_let_rec name value body e
  | Expr.Ref name -> Env.lookup name e
  | Expr.Fun { param; body } -> Fun { param; body; env = e }
  | Expr.Apply { f; arg } -> run_apply f arg e

and run_apply f arg e =
  match run_env f e with
  | Fun { param; body; env = e2 } ->
      let value = run_env arg e in
      run_env body (Env.bind param value e2)
  | _ -> failwith "type error in function application"

and run_let name value body e =
  let v = run_env value e in
  run_env body (Env.bind name v e)

and run_let_rec name value body e =
  let e2 = Env.reserve name e in
  let v = run_env value e2 in
  run_env body (Env.update name v e2)

and run_gt lhs rhs e =
  match (run_env lhs e, run_env rhs e) with
  | Num lhs, Num rhs -> Bool (lhs < rhs)
  | _ -> failwith "type error in <"

and run_add lhs rhs e =
  match (run_env lhs e, run_env rhs e) with
  | Num lhs, Num rhs -> Num (lhs + rhs)
  | _ -> failwith "type error in +"

and run_cond pred on_t on_f e =
  match run_env pred e with
  | Bool true -> run_env on_t e
  | Bool false -> run_env on_f e
  | _ -> failwith "type error in cond"

let run ?(env = Env.empty ()) expr = run_env expr env

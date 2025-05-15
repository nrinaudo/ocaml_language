type t =
  | Num of int
  | Bool of bool
  | Gt of { lhs : t; rhs : t }
  | Add of { lhs : t; rhs : t }
  | Cond of { pred : t; on_t : t; on_f : t }
  | Let of { name : string; value : t; body : t }
  | LetRec of { name : string; value : t; body : t }
  | Ref of string
  | Fun of { param : string; body : t }
  | Apply of { f : t; arg : t }

let rec pprint expr =
  match expr with
  | Num i -> string_of_int i
  | Bool b -> string_of_bool b
  | Ref name -> name
  | Gt { lhs; rhs } -> pprint_binary lhs "<" rhs
  | Add { lhs; rhs } -> pprint_binary lhs "+" rhs
  | Cond { pred; on_t; on_f } ->
      Printf.sprintf "if %s then %s else %s" (pprint pred) (pprint on_t)
        (pprint on_f)
  | Let { name; value; body } ->
      Printf.sprintf "let %s = %s in ( %s )" name (pprint value) (pprint body)
  | LetRec { name; value; body } ->
      Printf.sprintf "let rec %s = %s in ( %s )" name (pprint value)
        (pprint body)
  | Fun { param; body } -> Printf.sprintf "\\%s -> %s" param (pprint body)
  | Apply { f; arg } -> Printf.sprintf "%s %s" (pprint f) (pprint arg)

and pprint_binary lhs op rhs =
  Printf.sprintf "(%s %s %s)" (pprint lhs) op (pprint rhs)

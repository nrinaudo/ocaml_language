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

val pprint : t -> string

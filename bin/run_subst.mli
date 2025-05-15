module Value : sig
  type t =
    | Num of int
    | Bool of bool
    | Fun of { param : string; body : Expr.t }

  val pprint : t -> string
end

val run : Expr.t -> Value.t

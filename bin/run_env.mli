module Env : sig
  type 'a t

  val empty : unit -> 'a t
  (** Creates an empty environment. *)

  (* Creates a new environment in which the specified value is bound to the specified name *)
  val bind : string -> 'a -> 'a t -> 'a t

  (* Finds the value bound to the specified name *)
  val lookup : string -> 'a t -> 'a

  (* Creates a new environment in which space is reserved for the specified name, but not value is
     bound to it yet *)
  val reserve : string -> 'a t -> 'a t

  (* Updates the value bound to the specified name, mutating the environment rather than creating
     a new one *)
  val update : string -> 'a -> 'a t -> 'a t
end

module Value : sig
  type t =
    | Num of int
    | Bool of bool
    | Fun of { param : string; body : Expr.t; env : t Env.t }

  val pprint : t -> string
end

val run : ?env:Value.t Env.t -> Expr.t -> Value.t

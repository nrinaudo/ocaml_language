type 'a t
type 'a parsed

val map : ('a -> 'b) -> 'a t -> 'b t
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val ( * ) : 'a t -> 'b t -> ('a * 'b) t
val ( *> ) : 'a t -> 'b t -> 'b t
val ( <* ) : 'a t -> 'b t -> 'a t
val ( || ) : 'a t -> 'a t -> 'a t
val rep0 : 'a t -> 'a list t
val rep : 'a t -> 'a list t
val rep_sep0 : 'b t -> 'a t -> 'a list t
val rep_sep : 'b t -> 'a t -> 'a list t
val satisfy : (char -> bool) -> char t
val char : char -> char t
val one_of : 'a t list -> 'a t
val fail : string -> 'a t
val sequence : 'a t list -> 'a list t
val const : 'a -> 'a t
val parse : string -> 'a t -> ('a, string) result

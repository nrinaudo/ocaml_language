type 'a parsed = { state : string; value : 'a }
type 'a t = string -> ('a parsed, string) result

let satisfy pred =
  let drop_head str = (str.[0], String.sub str 1 (String.length str - 1)) in
  let uncons str = if str = String.empty then None else Some (drop_head str) in
  fun state ->
    match uncons state with
    | Some (actual, state') when pred actual ->
        Ok { state = state'; value = actual }
    | Some (actual, _) -> Error (Printf.sprintf "Unexpected input %c" actual)
    | None -> Error "Reached end of input"

let char c = satisfy (( = ) c)

let map f pa =
  let map_value result = { result with value = f result.value } in
  fun str -> Result.map map_value (pa str)

let map2 f pa pb =
 fun state ->
  match pa state with
  | Ok { state = state'; value = a } -> (map (fun b -> f a b) pb) state'
  | Error e -> Error e

let ( * ) pa pb = map2 (fun a b -> (a, b)) pa pb
let ( *> ) pa pb = map2 (fun _ b -> b) pa pb
let ( <* ) pa pb = map2 (fun a _ -> a) pa pb

let ( || ) pa pb =
 fun state -> match pa state with Ok ra -> Ok ra | _ -> pb state

let fail err = fun _ -> Error err
let const a = fun state -> Ok { state; value = a }

let rec sequence ps =
  match ps with
  | head :: tail -> map2 List.cons head (sequence tail)
  | _ -> const []

let one_of ps =
  match ps with
  | head :: tail -> List.fold_left ( || ) head tail
  | _ -> fail "Empty list of parsers"

let rec rep0 pa = rep pa || const []
and rep pa = map2 List.cons pa (rep0 pa)

let rec rep_sep0 sep pa = rep_sep sep pa || const []
and rep_sep sep pa = map2 List.cons pa (rep0 (sep *> pa))

let parse input p = Result.map (fun x -> x.value) (p input)

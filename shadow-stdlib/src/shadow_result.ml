(** Recreating 4.08's Result module in 4.06.1
    See: https://ocaml.org/releases/4.08/htmlman/libref/Result.html *)

type ('a, 'e) t = ('a, 'e) result

external%private js_to_string : 'a -> string = "toString" [@@bs.send]

let ok r = Ok r

let error e = Error e

let value r ~default = Belt.Result.getWithDefault r default

let get_ok r = Belt.Result.getExn r

let get_error r =
  match r with
  | Error e -> e
  | Ok v -> raise (Invalid_argument (js_to_string v))

let bind = Belt.Result.flatMap

let join rr = match rr with Ok r -> r | Error e -> Error e

let map f r = Belt.Result.map r f

let map_error_uncurried f r = match r with Ok r -> r | Error e -> f e [@bs]

let map_error f r = map_error_uncurried (fun [@bs] x -> f x) r

let fold_uncurried ~ok ~error r =
  match r with Ok r -> ok r [@bs] | Error r -> error r [@bs]

let fold ~ok ~error r =
  fold_uncurried ~ok:(fun [@bs] x -> ok x) ~error:(fun [@bs] x -> error x) r

let iter_uncurried f r = match r with Ok r -> f r [@bs] | Error _ -> ()

let iter f r = iter_uncurried (fun [@bs] x -> f x) r

let is_ok = Belt.Result.isOk

let is_error = Belt.Result.isError

let equal_uncurried ~ok ~error a b =
  match (a, b) with
  | Ok a, Ok b -> ok a b [@bs]
  | Error a, Error b -> error a b [@bs]
  | _ -> false

let equal ~ok ~error a b =
  equal_uncurried
    ~ok:(fun [@bs] a b -> ok a b)
    ~error:(fun [@bs] a b -> error a b)
    a b

let compare_uncurried ~ok ~error a b =
  match (a, b) with
    | Ok a, Ok b -> ok a b [@bs]
    | Error a, Error b -> error a b [@bs]
    | Ok _, Error _ -> -1
    | _ -> 1

let to_option r =
  match r with
    | Ok r -> Some r
    | _ -> None

let to_list r =
  match r with
    | Ok r -> [r]
    | _ -> []

let to_seq r = match r with Ok v -> Shadow_seq.return v | _ -> Shadow_seq.empty
(** Recreating 4.08's Int module in 4.06.1
    See: https://ocaml.org/releases/4.08/htmlman/libref/Option.html *)

type 'a t = 'a option

let none : 'a option = None

let some v = Some v

let value a ~default:d = Belt.Option.getWithDefault a d

let get a = Belt.Option.getExn a

let bind = Belt.Option.flatMap

let join v = match v with Some n -> n | None -> None

let map f a = Belt.Option.map a f

let fold ~none ~some b = Belt.Option.mapWithDefault b none some

let iter f a = Belt.Option.forEach a f

let is_none = Belt.Option.isNone

let is_some = Belt.Option.isSome

let equal f a b = Belt.Option.eq a b f

let compare f a b = Belt.Option.cmp a b f

let to_result ~none a = match a with Some v -> Ok v | None -> Error none

let to_list a = match a with Some v -> [ v ] | None -> []

let to_seq a = match a with Some v -> Shadow_seq.return v | None -> Shadow_seq.empty
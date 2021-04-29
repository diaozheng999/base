(** Recreating 4.08's Unit module in 4.06.1
    See: https://ocaml.org/releases/4.08/htmlman/libref/Unit.html *)

type t = unit

let equal () () = true

let compare () () = 0

let to_string () = "()"

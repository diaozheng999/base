(** Recreating 4.08's Bool module in 4.06.1
    See: https://ocaml.org/releases/4.08/htmlman/libref/Stdlib.Bool.html *)

type t = bool

(* These need to be declared as an external to get the lazy behavior *)
external ( && ) : bool -> bool -> bool = "%sequand"

external ( || ) : bool -> bool -> bool = "%sequor"

external not : bool -> bool = "%boolnot"

external equal : bool -> bool -> bool = "%eq"

let [@warning "-27"] compare (a : bool) (b : bool) : bool = [%raw "a - b"]

external to_int : bool -> int = "Number" [@@bs.val]

external to_float : bool -> float = "Number" [@@bs.val]

external to_string : bool -> string = "toString" [@@bs.send]

(** Recreating 4.08's Int module in 4.06.1
    See: https://ocaml.org/releases/4.08/htmlman/libref/Int.html *)

type t = int

let zero = 0 [@@inline]

let one = 1 [@@inline]

let minus_one = -1 [@@inline]

external neg : int -> int = "%negint"

external add : int -> int -> int = "%addint"

external sub : int -> int -> int = "%subint"

external mul : int -> int -> int = "%mulint"

external div : int -> int -> int = "%divint"

external rem : int -> int -> int = "%modint"

external succ : int -> int = "%succint"

external pred : int -> int = "%predint"

let abs = Pervasives.abs

let max_int = Pervasives.max_int

let max_float = Pervasives.max_float


external logand : int -> int -> int = "%andint"
external logor : int -> int -> int = "%orint"
external logxor : int -> int -> int = "%xorint"

let lognot = Pervasives.lnot


external shift_left : int -> int -> int = "%lslint"
external shift_right_logical : int -> int -> int = "%lsrint"
external shift_right : int -> int -> int = "%asrint"

external equal : int -> int -> bool = "%eq"

external compare : int -> int -> int = "%subint"

external to_float : int -> float = "%identity"

external of_float : float -> int = "%intoffloat"

external to_string : int -> string = "toString" [@@bs.send]

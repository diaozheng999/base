include Int_intf.Int (** @inline *)


include Comparisons.Infix_external with type t := t

include Int_intf.Operators_external_int

external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external incr : int ref -> unit = "%incr"
external decr : int ref -> unit = "%decr"
external bit_and : t -> t -> t = "%andint"
external bit_or : t -> t -> t = "%orint"
external bit_xor : t -> t -> t = "%xorint"
external shift_left : t -> t -> t = "%lslint"
external shift_right : t -> t -> t = "%asrint"
external shift_right_logical : t -> t -> t = "%lsrint"
external of_int : t -> t = "%identity"
external to_int : t -> t = "%identity"
external of_int32 : int32 -> t option = "%int32_to_int"
external to_int32 : t -> int32 option = "%int32_of_int"
external of_int32_exn : int32 -> t = "%int32_to_int"
external to_int32_exn : t -> int32 = "%int32_of_int"
external of_int_exn : t -> t = "%identity"
external to_int_exn : t -> t = "%identity"
external of_float_unchecked : float -> t = "%intoffloat"
external to_float : t -> float = "%identity"
external to_string : t -> string = "toString" [@@bs.send]
external of_string : string -> t = "parseInt" [@@bs.val]
external bswap16 : int -> int = "bswap16"
    [@@bs.module "@nasi/js-base-runtime"]
    [@@bs.scope "int"]
external clz: int -> (int[@untagged]) = "clz"
    [@@bs.module "@nasi/js-base-runtime"]
    [@@bs.scope "int"]
external ctz: (int[@untagged]) -> (int[@untagged]) = "ctz"
    [@@bs.module "@nasi/js-base-runtime"]
    [@@bs.scope "int"]

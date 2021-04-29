(** [state] is defined as a subtype of [int] using the [private] keyword. This makes it an
    opaque type for most purposes, and tells the compiler that the type is immediate. *)
type state = private int
type seed = int
type hash_value = int

external create_seeded  : seed            -> state = "%identity"                 
external fold_int64     : state -> int64  -> state = "foldInt64"     [@@bs.module "@nasi/js-base-runtime"][@@bs.scope "internalHash"]
external fold_int       : state -> int    -> state = "foldInt"       [@@bs.module "@nasi/js-base-runtime"][@@bs.scope "internalHash"]
external fold_float     : state -> float  -> state = "foldFloat"      [@@bs.module "@nasi/js-base-runtime"][@@bs.scope "internalHash"]
external fold_string    : state -> string -> state = "foldString"     [@@bs.module "@nasi/js-base-runtime"][@@bs.scope "internalHash"]
external get_hash_value : state -> hash_value      = "getHashValue"  [@@bs.module "@nasi/js-base-runtime"][@@bs.scope "internalHash"]

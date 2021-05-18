(** An int of exactly 32 bits, regardless of the machine.

    Side note: There's not much reason to want an int of at least 32 bits (i.e., 32 on
    32-bit machines and 63 on 64-bit machines) because [Int63] is basically just as
    efficient.

    Overflow issues are {i not} generally considered and explicitly handled.  This may be
    more of an issue for 32-bit ints than 64-bit ints.

    [Int32.t] is boxed on both 32-bit and 64-bit machines. *)

open! Import
include Int_intf.S with type t = int32

include Int_intf.Operators_external_int32

(** {2 Conversion functions} *)

val of_nativeint : int32 -> t option
val to_nativeint : t -> int32
val of_int64 : int64 -> t option
external of_int : int -> t option = "%int32_of_int"
external to_int : t -> int option = "%int32_to_int"
external of_int32 : int32 -> t option = "%identity"
external to_int32 : t -> int32 option = "%identity"
external of_int32_exn : int32 -> t = "%identity"
external to_int32_exn : t -> int32 = "%identity"
external of_int_exn : int -> t = "%int32_of_int"
external to_int_exn : t -> int = "%int32_to_int"

(** {3 Truncating conversions}

    These functions return the least-significant bits of the input. In cases where
    optional conversions return [Some x], truncating conversions return [x]. *)

val of_int_trunc : int -> t
val to_int_trunc : t -> int
val of_nativeint_trunc : int32 -> t
val of_int64_trunc : int64 -> t

(** {3 Low-level float conversions} *)

(** Rounds a regular 64-bit OCaml float to a 32-bit IEEE-754 "single" float, and returns
    its bit representation.  We make no promises about the exact rounding behavior, or
    what happens in case of over- or underflow. *)
external bits_of_float : float -> t
    = "caml_int32_bits_of_float" "caml_int32_bits_of_float_unboxed"
    [@@unboxed] [@@noalloc]

(** Creates a 32-bit IEEE-754 "single" float from the given bits, and converts it to a
    regular 64-bit OCaml float. *)
external float_of_bits : t -> float
    = "caml_int32_float_of_bits" "caml_int32_float_of_bits_unboxed"
    [@@unboxed] [@@noalloc]

(** {2 Byte swap operations}

    See {{!modtype:Int.Int_without_module_types}[Int]'s byte swap section} for
    a description of Base's approach to exposing byte swap primitives.

    When compiling for 64-bit machines, if signedness of the output value does not matter,
    use byteswap functions for [int64], if possible, for better performance. As of
    writing, 32-bit byte swap operations on 64-bit machines have extra overhead for moving
    to 32-bit registers and sign-extending values when returning to 64-bit registers.

    The x86 instruction sequence that demonstrates the overhead is in
    [base/bench/bench_int.ml] *)

val bswap16 : t -> t
external bswap32 : t -> t = "bswap32"
  [@@bs.module "@nasi/js-base-runtime"]
  [@@bs.scope "int"]

external bit_and : t -> t -> t = "%int32_and"
external bit_or : t -> t -> t = "%int32_or"
external bit_xor : t -> t -> t = "%%int32_xor"
external shift_left : t -> t -> t = "%int32_lsl"
external shift_right : t -> t -> t = "%int32_asr"
external shift_right_logical : t -> t -> t = "%int32_lsr"
external to_float : int32 -> float
  = "caml_int32_to_float" "caml_int32_to_float_unboxed"
  [@@unboxed] [@@noalloc]

external clz
    :  (int32[@unboxed])
    -> (int[@untagged])
    = "clz_int32"
  [@@bs.module "@nasi/js-base-runtime"]
  [@@bs.scope "int"]

external ctz
    :  (int32[@unboxed])
    -> (int[@untagged])
    = "ctz_int32"
  [@@bs.module "@nasi/js-base-runtime"]
  [@@bs.scope "int"]

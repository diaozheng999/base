(** Recreating 4.08's Float module in 4.06.1
    See: https://ocaml.org/releases/4.08/htmlman/libref/Float.html *)

type t = float

exception Not_implemented

let zero = 0. [@@inline]

let one = 1. [@@inline]

let minus_one = -1. [@@inline]

external neg : float -> float = "%negfloat"

external add : float -> float -> float = "%addfloat"

external sub : float -> float -> float = "%subfloat"

external mul : float -> float -> float = "%mulfloat"

external div : float -> float -> float = "%divfloat"

(* This is not optimised *)
let fma x y z = (x *. y) +. z

external rem : float -> float -> float = "caml_fmod_float" "fmod"
  [@@unboxed] [@@noalloc]

let succ : float -> float = fun _ -> raise Not_implemented

let pred : float -> float = fun _ -> raise Not_implemented

external abs : float -> float = "abs" [@@bs.val] [@@bs.scope "Math"]

external infinity : float = "Infinity" [@@bs.val]

external neg_infinity : float = "NEGATIVE_INFINITY"
  [@@bs.val] [@@bs.scope "Number"]

external pi : float = "PI" [@@bs.val] [@@bs.scope "Math"]

external max_float : float = "MAX_VALUE" [@@bs.val] [@@bs.scope "Number"]

external min_float : float = "MAX_VALUE" [@@bs.val] [@@bs.scope "Number"]

external epsilon : float = "EPSILON" [@@bs.val] [@@bs.scope "Number"]

external is_finite : float -> bool = "isFinite" [@@bs.val]

let is_infinite f = not (is_finite f)

external is_nan : float -> bool = "isNaN" [@@bs.val]

external of_int : int -> float = "%identity"

external to_int : float -> int = "%intoffloat"

external of_string : string -> float = "parseFloat" [@@bs.val]

let of_string_opt = Belt.Float.fromString

external to_string : float -> string = "toString" [@@bs.send]

type fpclass = Pervasives.fpclass

let classify_float = Pervasives.classify_float

external pow : float -> float -> float = "pow" [@@bs.val] [@@bs.scope "Math"]

external sqrt : float -> float = "sqrt" [@@bs.val] [@@bs.scope "Math"]

external exp : float -> float = "exp" [@@bs.val] [@@bs.scope "Math"]

external log : float -> float = "log" [@@bs.val] [@@bs.scope "Math"]

external log10 : float -> float = "log10" [@@bs.val] [@@bs.scope "Math"]

external expm1 : float -> float = "expm1" [@@bs.val] [@@bs.scope "Math"]

external log1p : float -> float = "log1p" [@@bs.val] [@@bs.scope "Math"]

external cos : float -> float = "cos" [@@bs.val] [@@bs.scope "Math"]

external sin : float -> float = "sin" [@@bs.val] [@@bs.scope "Math"]

external tan : float -> float = "tan" [@@bs.val] [@@bs.scope "Math"]

external acos : float -> float = "acos" [@@bs.val] [@@bs.scope "Math"]

external asin : float -> float = "asin" [@@bs.val] [@@bs.scope "Math"]

external atan : float -> float = "atan" [@@bs.val] [@@bs.scope "Math"]

external atan2 : float -> float -> float = "atan2" [@@bs.val] [@@bs.scope "Math"]

external hypot : float -> float -> float = "hypot" [@@bs.val] [@@bs.scope "Math"]

external cosh : float -> float = "cosh" [@@bs.val] [@@bs.scope "Math"]

external sinh : float -> float = "sinh" [@@bs.val] [@@bs.scope "Math"]

external tanh : float -> float = "tanh" [@@bs.val] [@@bs.scope "Math"]

external trunc : float -> float = "trunc" [@@bs.val] [@@bs.scope "Math"]

external round : float -> float = "round" [@@bs.val] [@@bs.scope "Math"]

external ceil : float -> float = "ceil" [@@bs.val] [@@bs.scope "Math"]

external floor : float -> float = "floor" [@@bs.val] [@@bs.scope "Math"]

let next_after = succ

external copy_sign : float -> float -> float
  = "caml_copysign_float" "caml_copysign"
  [@@unboxed] [@@noalloc]

external copysign : float -> float -> float
  = "caml_copysign_float" "caml_copysign"
  [@@unboxed] [@@noalloc]

external frexp : float -> float * int = "caml_frexp_float"

external ldexp : (float[@unboxed]) -> (int[@untagged]) -> (float[@unboxed])
  = "caml_ldexp_float" "caml_ldexp_float_unboxed"
  [@@noalloc]

external modf : float -> float * float = "caml_modf_float"

let[@warning "-27"] sign_bit (f : float) : bool = [%raw "!!Math.sign(f)"]

external compare : t -> t -> int = "%compare"

external equal : t -> t -> bool = "%eq"

external min : float -> float -> float = "min" [@@bs.val] [@@bs.scope "Math"]

external max : float -> float -> float = "max" [@@bs.val] [@@bs.scope "Math"]

let min_max a b = if a > b then (b, a) else (a, b)

let min_num a b =
  match (is_nan a, is_nan b) with
  | true, true -> nan
  | _, true -> a
  | true, _ -> b
  | _ -> min a b

let max_num a b =
  match (is_nan a, is_nan b) with
  | true, true -> nan
  | _, true -> a
  | true, _ -> b
  | _ -> max a b

let min_max_num a b =
  match (is_nan a, is_nan b) with
  | true, true -> (nan, nan)
  | _, true -> (a, a)
  | true, _ -> (b, b)
  | _ -> min_max a b

external hash : float -> int = "hashFloat"
  [@@bs.module "@nasi/js-base-runtime"]
  [@@bs.scope "shadowLib"]

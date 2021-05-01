(* [Int0] defines integer functions that are primitives or can be simply
   defined in terms of [Caml]. [Int0] is intended to completely express the
   part of [Caml] that [Base] uses for integers -- no other file in Base other
   than int0.ml should use these functions directly through [Caml]. [Int0] has
   few dependencies, and so is available early in Base's build order.

   All Base files that need to use ints and come before [Base.Int] in build
   order should do:

   {[
     module Int  = Int0
   ]}

   Defining [module Int = Int0] is also necessary because it prevents ocamldep
   from mistakenly causing a file to depend on [Base.Int]. *)

open Shadow

external to_string : int -> string = "toString" [@@bs.send]
external of_string : string -> int = "parseInt" [@@bs.val]
external to_float : int -> float = "%identity"

external of_float: float -> int = "%intoffloat"
let max_value = Caml.max_int
let min_value = Caml.min_int
external succ : int -> int = "%succint"

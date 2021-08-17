(** Same semantics as ['a Array.t], except it's guaranteed that the representation array
    is not tagged with [Double_array_tag], the tag for float arrays.

    This means it's safer to use in the presence of [Obj.magic], but it's slower than
    normal [Array] if you use it with floats.

    It can often be faster than [Array] if you use it with non-floats. *)


open! Import

(** See [Base.Array] for comments. *)
type 'a t [@@deriving_inline sexp, sexp_grammar]

include Ppx_sexp_conv_lib.Sexpable.S1 with type 'a t := 'a t

val t_sexp_grammar
  :  'a Ppx_sexp_conv_lib.Sexp_grammar.t
  -> 'a t Ppx_sexp_conv_lib.Sexp_grammar.t

[@@@end]

val invariant : _ t -> unit


val empty : _ t
val create : len:int -> 'a -> 'a t
val singleton : 'a -> 'a t
val init : int -> f:(int -> 'a) -> 'a t
external get : 'a t -> int -> 'a = "%array_safe_get"
external unsafe_get : 'a t -> int -> 'a = "%array_unsafe_get"
external set : 'a t -> int -> 'a -> unit = "%array_safe_set"
external unsafe_set : 'a t -> int -> 'a -> unit = "%array_unsafe_set"
external length : 'a t -> int = "%array_length"
val swap : _ t -> int -> int -> unit

(** [unsafe_set_omit_phys_equal_check] is like [unsafe_set], except it doesn't do a
    [phys_equal] check to try to skip [caml_modify].  It is safe to call this even if the
    values are [phys_equal]. *)
val unsafe_set_omit_phys_equal_check : 'a t -> int -> 'a -> unit

(** [unsafe_set_with_caml_modify] always calls [caml_modify] before setting and never gets
    the old value.  This is like [unsafe_set_omit_phys_equal_check] except it doesn't
    check whether the old value and the value being set are integers to try to skip
    [caml_modify]. *)
val unsafe_set_with_caml_modify : 'a t -> int -> 'a -> unit

external map : 'a t -> f:('a -> 'b[@bs.uncurry]) -> 'b t = "map" [@@bs.send]
external iter : 'a t -> f:('a -> unit[@bs.uncurry]) -> unit = "forEach" [@@bs.send]

(** Like {!iter}, but the function is applied to the index of the element as first
    argument, and the element itself as second argument. *)
val iteri : 'a t -> f:(int -> 'a -> unit) -> unit

(** [of_array] and [to_array] return fresh arrays with the same contents rather than
    returning a reference to the underlying array. *)
external of_array : 'a array -> 'a t = "slice" [@@bs.send]

external to_array : 'a t -> 'a array = "slice" [@@bs.send]
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list

include Blit.S1 with type 'a t := 'a t

external copy : 'a t -> 'a t = "slice" [@@bs.send]

(** {2 Extra lowlevel and unsafe functions} *)

(** The behavior is undefined if you access an element before setting it. *)
external unsafe_create_uninitialized : len:int -> _ t = "Array" [@@bs.new]

(** New obj array filled with [Obj.repr 0] *)
val create_obj_array : len:int -> Caml.Obj.t t

(** [unsafe_set_assuming_currently_int t i obj] sets index [i] of [t] to [obj], but only
    works correctly if the value there is an immediate, i.e. [Caml.Obj.is_int (get t i)].
    This precondition saves a dynamic check.

    [unsafe_set_int_assuming_currently_int] is similar, except the value being set is an
    int.

    [unsafe_set_int] is similar but does not assume anything about the target. *)
val unsafe_set_assuming_currently_int : Caml.Obj.t t -> int -> Caml.Obj.t -> unit

val unsafe_set_int_assuming_currently_int : Caml.Obj.t t -> int -> int -> unit
val unsafe_set_int : Caml.Obj.t t -> int -> int -> unit


(** [unsafe_clear_if_pointer t i] prevents [t.(i)] from pointing to anything to prevent
    space leaks.  It does this by setting [t.(i)] to [Caml.Obj.repr 0].  As a performance
    hack, it only does this when [not (Caml.Obj.is_int t.(i))].  It is an error to access
    the cleared index before setting it again. *)
val unsafe_clear_if_pointer : Caml.Obj.t t -> int -> unit

(** As [Array.exists]. *)
external exists : 'a t -> f:('a -> bool[@bs.uncurry]) -> bool = "some" [@@bs.send]

(** Functions with the 2 suffix raise an exception if the lengths of the two given arrays
    aren't the same. *)
val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

val min_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option
val max_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option

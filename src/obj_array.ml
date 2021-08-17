open! Import
module Int = Int0
module String = String0
module Array = Array0

(* We maintain the property that all values of type [t] do not have the tag
   [double_array_tag].  Some functions below assume this in order to avoid testing the
   tag, and will segfault if this property doesn't hold. *)
type t = Caml.Obj.t array

(* Skip this invariant for now. We're not dealing with typed arrays yet. *)
let invariant _t = assert (true)
external length : t -> int = "%array_length"
let swap t i j = Array.swap t i j

let sexp_of_t t =
  Sexp.Atom
    (String.concat ~sep:"" [ "<Obj_array.t of length "; Int.to_string (length t); ">" ])
;;

let zero_obj = Caml.Obj.repr (0 : int)

external create : len:int -> Caml.Obj.t -> t = "caml_make_vect"

(* We call [Array.create] with a value that is not a float so that the array doesn't get
   tagged with [Double_array_tag]. *)
let create_zero ~len = create ~len zero_obj

external create_empty : len:int -> t = "Array" [@@bs.new]

let empty = [||]

type not_a_float =
  | Not_a_float_0
  | Not_a_float_1 of int

let _not_a_float_0 = Not_a_float_0
let _not_a_float_1 = Not_a_float_1 42

(* Melange: We leave the boxing to the respective JS engine, this is simply
   a passthrough for array functions. *)

external get : t -> int -> Obj.t = "%array_safe_get"

external unsafe_get : t -> int -> Obj.t = "%array_unsafe_get"

external set : t -> int -> Obj.t -> unit = "%array_safe_set"

external unsafe_set : t -> int -> Obj.t -> unit = "%array_unsafe_set"

let[@inline always] unsafe_set_with_caml_modify t i obj =
  (* Same comment as [unsafe_get]. Sys.opaque_identity prevents the compiler from
     potentially wrongly guessing the type of the array based on the type of element, that
     is prevent the implication: (Obj.tag obj = Obj.double_tag) => (Obj.tag t =
     Obj.double_array_tag) which flambda has tried in the past (at least that's assuming
     the compiler respects Sys.opaque_identity, which is not always the case). *)
  Array.unsafe_set
    (Caml.Obj.magic (t : t) : not_a_float array)
    i
    (Caml.Obj.obj (Sys.opaque_identity obj) : not_a_float)
;;

let[@inline always] unsafe_set_int_assuming_currently_int t i int =
  (* This skips [caml_modify], which is OK if both the old and new values are integers. *)
  Array.unsafe_set (Caml.Obj.magic (t : t) : int array) i (Sys.opaque_identity int)
;;

let unsafe_is_int obj = Js.typeof obj == "number"

(** Melange: The ocamlopt optimisations below doesn't do much when targeting
    Melange, leaving these functions here for compatibility. *)
let[@inline always] unsafe_set_omit_phys_equal_check t i obj =
  let old_obj = unsafe_get t i in
  if unsafe_is_int old_obj && unsafe_is_int obj
  then unsafe_set_int_assuming_currently_int t i (Caml.Obj.obj obj : int)
  else unsafe_set_with_caml_modify t i obj
;;

let singleton obj = create ~len:1 obj

(* Pre-condition: t.(i) is an integer. *)
let unsafe_set_assuming_currently_int t i obj =
  if unsafe_is_int obj
  then unsafe_set_int_assuming_currently_int t i (Caml.Obj.obj obj : int)
  else
    (* [t.(i)] is an integer and [obj] is not, so we do not need to check if they are
       equal. *)
    unsafe_set_with_caml_modify t i obj
;;

let unsafe_set_int t i int =
  let old_obj = unsafe_get t i in
  if unsafe_is_int old_obj
  then unsafe_set_int_assuming_currently_int t i int
  else unsafe_set_with_caml_modify t i (Caml.Obj.repr int)
;;

let unsafe_clear_if_pointer t i =
  let old_obj = unsafe_get t i in
  if not (unsafe_is_int old_obj) then unsafe_set_with_caml_modify t i (Caml.Obj.repr 0)
;;

(** [unsafe_blit] is like [Array.blit], except it uses our own for-loop to avoid
    caml_modify when possible.  Its performance is still not comparable to a memcpy. *)
let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
  (* When [phys_equal src dst], we need to check whether [dst_pos < src_pos] and have the
     for loop go in the right direction so that we don't overwrite data that we still need
     to read.  When [not (phys_equal src dst)], doing this is harmless.  From a
     memory-performance perspective, it doesn't matter whether one loops up or down.
     Constant-stride access, forward or backward, should be indistinguishable (at least on
     an intel i7).  So, we don't do a check for [phys_equal src dst] and always loop up in
     that case. *)
  if dst_pos < src_pos
  then
    for i = 0 to len - 1 do
      unsafe_set dst (dst_pos + i) (unsafe_get src (src_pos + i))
    done
  else
    for i = len - 1 downto 0 do
      unsafe_set dst (dst_pos + i) (unsafe_get src (src_pos + i))
    done
;;

include Blit.Make (struct
    type nonrec t = t

    let create = create_zero
    let length = length
    let unsafe_blit = unsafe_blit
  end)

external copy : t -> t = "slice" [@@bs.send]

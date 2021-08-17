open! Import

module Array = Array_base

(* WARNING:
   We use non-memory-safe things throughout the [Trusted] module.
   Most of it is only safe in combination with the type signature (e.g. exposing
   [val copy : 'a t -> 'b t] would be a big mistake). *)
module Trusted : sig
  type 'a t

  external unsafe_create_uninitialized : len:int -> 'a t = "Array" [@@bs.new]
  external create : len:int -> 'a -> 'a t = "caml_make_vect"
  external copy : 'a t -> 'a t = "slice" [@@bs.send]
  external get : 'a t -> int -> 'a = "%array_safe_get"
  external unsafe_get : 'a t -> int -> 'a = "%array_unsafe_get"
  external set : 'a t -> int -> 'a -> unit = "%array_safe_set"
  external unsafe_set : 'a t -> int -> 'a -> unit = "%array_unsafe_set"
  external length : 'a t -> int = "%array_length"

  val empty : 'a t
  val create_obj_array : len:int -> 'a t
  val singleton : 'a -> 'a t
  val swap : _ t -> int -> int -> unit
  val unsafe_set_omit_phys_equal_check : 'a t -> int -> 'a -> unit
  val unsafe_set_int : 'a t -> int -> int -> unit
  val unsafe_set_int_assuming_currently_int : 'a t -> int -> int -> unit
  val unsafe_set_assuming_currently_int : 'a t -> int -> 'a -> unit
  val unsafe_set_with_caml_modify : 'a t -> int -> 'a -> unit
  val unsafe_blit : ('a t, 'a t) Blit.blit
  val unsafe_clear_if_pointer : _ t -> int -> unit
end = struct
  type 'a t = Obj_array.t

  external unsafe_create_uninitialized : len:int -> 'a t = "Array" [@@bs.new]
  external create : len:int -> 'a -> 'a t = "caml_make_vect"
  external copy : 'a t -> 'a t = "slice" [@@bs.send]
  external get : 'a t -> int -> 'a = "%array_safe_get"
  external unsafe_get : 'a t -> int -> 'a = "%array_unsafe_get"
  external set : 'a t -> int -> 'a -> unit = "%array_safe_set"
  external unsafe_set : 'a t -> int -> 'a -> unit = "%array_unsafe_set"
  external length : 'a t -> int = "%array_length"

  let empty = Obj_array.empty
  let create_obj_array ~len = Obj_array.create_zero ~len
  let singleton x = Obj_array.singleton (Caml.Obj.repr x)
  let swap t i j = Obj_array.swap t i j  let unsafe_set_int arr i x = Obj_array.unsafe_set_int arr i x

  let unsafe_set_int_assuming_currently_int arr i x =
    Obj_array.unsafe_set_int_assuming_currently_int arr i x
  ;;

  let unsafe_set_assuming_currently_int arr i x =
    Obj_array.unsafe_set_assuming_currently_int arr i (Caml.Obj.repr x)
  ;;

  let unsafe_blit = Obj_array.unsafe_blit

  let unsafe_set_omit_phys_equal_check t i x =
    Obj_array.unsafe_set_omit_phys_equal_check t i (Caml.Obj.repr x)
  ;;

  let unsafe_set_with_caml_modify t i x =
    Obj_array.unsafe_set_with_caml_modify t i (Caml.Obj.repr x)
  ;;

  let unsafe_clear_if_pointer = Obj_array.unsafe_clear_if_pointer
end

include Trusted

let invariant _t = assert (true)

let init l ~f =
  if l < 0
  then invalid_arg "Uniform_array.init"
  else (
    let res = unsafe_create_uninitialized ~len:l in
    for i = 0 to l - 1 do
      unsafe_set res i (f i)
    done;
    res)
;;

external of_array : 'a array -> 'a t = "slice" [@@bs.send]

let map a ~f = init ~f:(fun i -> f (unsafe_get a i)) (length a)

external map : 'a t -> f:('a -> 'b[@bs.uncurry]) -> 'b t = "map" [@@bs.send]

external iter : 'a t -> f:('a -> unit[@bs.uncurry]) -> unit = "forEach" [@@bs.send]

let iteri a ~f =
  for i = 0 to length a - 1 do
    f i (unsafe_get a i)
  done
;;

let to_list t = List.init ~f:(get t) (length t)

let of_list l =
  let len = List.length l in
  let res = unsafe_create_uninitialized ~len in
  List.iteri l ~f:(fun i x -> set res i x);
  res
;;

(* It is not safe for [to_array] to be the identity function because we have code that
   relies on [float array]s being unboxed, for example in [bin_write_array]. *)
external to_array : 'a t -> 'a array = "slice" [@@bs.send]

external exists : 'a t -> f:('a -> bool[@bs.uncurry]) -> bool = "some" [@@bs.send]

let map2_exn t1 t2 ~f =
  let len = length t1 in
  if length t2 <> len then invalid_arg "Array.map2_exn";
  init len ~f:(fun i -> f (unsafe_get t1 i) (unsafe_get t2 i))
;;

let t_sexp_grammar (type elt) (grammar : elt Ppx_sexp_conv_lib.Sexp_grammar.t)
  : elt t Ppx_sexp_conv_lib.Sexp_grammar.t
  =
  Ppx_sexp_conv_lib.Sexp_grammar.coerce (Array.t_sexp_grammar grammar)
;;

include
  Sexpable.Of_sexpable1
    (Array)
    (struct
      type nonrec 'a t = 'a t

      let to_sexpable = to_array
      let of_sexpable = of_array
    end)

include Blit.Make1 (struct
    type nonrec 'a t = 'a t

    let length = length

    let create_like ~len t =
      if len = 0
      then empty
      else (
        assert (length t > 0);
        create ~len (get t 0))
    ;;

    let unsafe_blit = unsafe_blit
  end)

external reduce : 'a t ->  ('b -> 'a  -> 'b [@bs.uncurry]) -> 'b -> 'b = "reduce" [@@bs.send]

let fold t ~init ~f = reduce t f init

let min_elt t ~compare = Container.min_elt ~fold t ~compare
let max_elt t ~compare = Container.max_elt ~fold t ~compare

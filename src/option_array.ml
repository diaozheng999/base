open! Import

(** ['a Cheap_option.t] is like ['a option], but it doesn't box [some _] values.

    There are several things that are unsafe about it:

    - [float t array] (or any array-backed container) is not memory-safe
      because float array optimization is incompatible with unboxed option
      optimization. You have to use [Uniform_array.t] instead of [array].

    - Nested options (['a t t]) don't work. They are believed to be
      memory-safe, but not parametric.

    - A record with [float t]s in it should be safe, but it's only [t] being
      abstract that gives you safety. If the compiler was smart enough to peek
      through the module signature then it could decide to construct a float
      array instead. *)
module Cheap_option = struct

  module T0 : sig
    type 'a t

    val none : _ t
    val some : 'a -> 'a t
    val is_none : _ t -> bool
    val is_some : _ t -> bool
    val value_exn : 'a t -> 'a
    val value_unsafe : 'a t -> 'a
  end = struct
    type +'a t

    (* Being a pointer, no one outside this module can construct a value that is
       [phys_same] as this one.

       It would be simpler to use this value as [none], but we use an immediate instead
       because it lets us avoid caml_modify when setting to [none], making certain
       benchmarks significantly faster (e.g. ../bench/array_queue.exe).

       this code is duplicated in Moption, and if we find yet another place where we want
       it we should reconsider making it shared. *)

    let none_substitute : _ t =
      (* The number was produced by
         [< /dev/urandom tr -c -d '1234567890abcdef' | head -c 16].

         The idea is that a random number will have lower probability to collide with
         anything than any number we can choose ourselves.

         We are using a polymorphic variant instead of an integer constant because there
         is a compiler bug where it wrongly assumes that the result of [if _ then c else
         y] is not a pointer if [c] is an integer compile-time constant.  This is being
         fixed in https://github.com/ocaml/ocaml/pull/555.  The "memory corruption" test
         below demonstrates the issue.  *)
      Caml.Obj.magic 0x6e8ee347
    ;;

    external none : _ t = "undefined" [@@bs.val]

    let is_none x = x == none
    let is_some x = x != none

    let [@warning "-27"] unsafe_is_none x = [%raw "x === undefined"]

    let some (type a) (x : a) : a t =
      if unsafe_is_none x then none else Caml.Obj.magic x
    ;;

    let value_unsafe (type a) (x : a t) : a =
      if x == none_substitute then Caml.Obj.magic none else Caml.Obj.magic x
    ;;

    let value_exn x =
      if is_some x
      then value_unsafe x
      else failwith "Option_array.get_some_exn: the element is [None]"
    ;;
  end

  module T1 = struct
    include T0

    let of_option = function
      | None -> none
      | Some x -> some x
    ;;

    let to_option x = if is_some x then Some (value_unsafe x) else None
    let to_sexpable = to_option
    let of_sexpable = of_option

    let t_sexp_grammar (type a) (grammar : a Ppx_sexp_conv_lib.Sexp_grammar.t)
      : a t Ppx_sexp_conv_lib.Sexp_grammar.t
      =
      Ppx_sexp_conv_lib.Sexp_grammar.coerce (Option.t_sexp_grammar grammar)
    ;;
  end

  include T1
  include Sexpable.Of_sexpable1 (Option) (T1)
end

type 'a t = 'a Cheap_option.t Uniform_array.t [@@deriving_inline sexp, sexp_grammar]

let t_of_sexp : 'a. (Ppx_sexp_conv_lib.Sexp.t -> 'a) -> Ppx_sexp_conv_lib.Sexp.t -> 'a t =
  let _tp_loc = "option_array.ml.t" in
  fun _of_a t -> Uniform_array.t_of_sexp (Cheap_option.t_of_sexp _of_a) t
;;

let sexp_of_t : 'a. ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t =
  fun _of_a v -> Uniform_array.sexp_of_t (Cheap_option.sexp_of_t _of_a) v
;;

let (t_sexp_grammar :
       'a Ppx_sexp_conv_lib.Sexp_grammar.t -> 'a t Ppx_sexp_conv_lib.Sexp_grammar.t)
  =
  fun _'a_sexp_grammar ->
  Uniform_array.t_sexp_grammar (Cheap_option.t_sexp_grammar _'a_sexp_grammar)
;;

[@@@end]

let empty = Uniform_array.empty
let create ~len = Uniform_array.create ~len Cheap_option.none
let init n ~f = Uniform_array.init n ~f:(fun i -> Cheap_option.of_option (f i))
let init_some n ~f = Uniform_array.init n ~f:(fun i -> Cheap_option.some (f i))
let length = Uniform_array.length
let get t i = Cheap_option.to_option (Uniform_array.get t i)
let get_some_exn t i = Cheap_option.value_exn (Uniform_array.get t i)
let is_none t i = Cheap_option.is_none (Uniform_array.get t i)
let is_some t i = Cheap_option.is_some (Uniform_array.get t i)
let set t i x = Uniform_array.set t i (Cheap_option.of_option x)
let set_some t i x = Uniform_array.set t i (Cheap_option.some x)
let set_none t i = Uniform_array.set t i Cheap_option.none
let swap t i j = Uniform_array.swap t i j
let unsafe_get t i = Cheap_option.to_option (Uniform_array.unsafe_get t i)
let unsafe_get_some_exn t i = Cheap_option.value_exn (Uniform_array.unsafe_get t i)

let unsafe_get_some_assuming_some t i =
  Cheap_option.value_unsafe (Uniform_array.unsafe_get t i)
;;

let unsafe_is_some t i = Cheap_option.is_some (Uniform_array.unsafe_get t i)
let unsafe_set t i x = Uniform_array.unsafe_set t i (Cheap_option.of_option x)
let unsafe_set_some t i x = Uniform_array.unsafe_set t i (Cheap_option.some x)
let unsafe_set_none t i = Uniform_array.unsafe_set t i Cheap_option.none

let clear t =
  for i = 0 to length t - 1 do
    unsafe_set_none t i
  done
;;

include Blit.Make1_generic (struct
    type nonrec 'a t = 'a t

    let length = length
    let create_like ~len _ = create ~len
    let unsafe_blit = Uniform_array.unsafe_blit
  end)

let copy = Uniform_array.copy

module For_testing = struct
  module Unsafe_cheap_option = Cheap_option
end

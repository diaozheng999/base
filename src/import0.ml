(* This module is included in [Import].  It is aimed at modules that define the standard
   combinators for [sexp_of], [of_sexp], [compare] and [hash] and are included in
   [Import]. *)
open Shadow

include (
  Shadow_stdlib :
    module type of struct
    include Shadow_stdlib
  end
  (* These modules are redefined in Base *)
  with module Array := Shadow_stdlib.Array
  with module Bool := Shadow_stdlib.Bool
  with module Buffer := Shadow_stdlib.Buffer
  with module Bytes := Shadow_stdlib.Bytes
  with module Char := Shadow_stdlib.Char
  with module Float := Shadow_stdlib.Float
  with module Hashtbl := Shadow_stdlib.Hashtbl
  with module Int := Shadow_stdlib.Int
  with module Int32 := Shadow_stdlib.Int32
  with module Int64 := Shadow_stdlib.Int64
  with module Lazy := Shadow_stdlib.Lazy
  with module List := Shadow_stdlib.List
  with module Map := Shadow_stdlib.Map
  with module Nativeint := Shadow_stdlib.Nativeint
  with module Option := Shadow_stdlib.Option
  with module Printf := Shadow_stdlib.Printf
  with module Queue := Shadow_stdlib.Queue
  with module Random := Shadow_stdlib.Random
  with module Result := Shadow_stdlib.Result
  with module Set := Shadow_stdlib.Set
  with module Stack := Shadow_stdlib.Stack
  with module String := Shadow_stdlib.String
  with module Sys := Shadow_stdlib.Sys
  with module Uchar := Shadow_stdlib.Uchar
  with module Unit := Shadow_stdlib.Unit) [@ocaml.warning "-3"]

type 'a ref = 'a Caml.ref = { mutable contents : 'a }

(* Reshuffle [Caml] so that we choose the modules using labels when available. *)
module Caml = struct

  module Arg = Caml.Arg (** @canonical Caml.Arg *)

  module Array = Caml.StdLabels.Array (** @canonical Caml.StdLabels.Array *)

  module Bool = Caml.Bool (** @canonical Caml.Bool *)

  module Buffer = Caml.Buffer (** @canonical Caml.Buffer *)

  module Bytes = Caml.StdLabels.Bytes (** @canonical Caml.StdLabels.Bytes *)

  module Char = Caml.Char (** @canonical Caml.Char *)

  module Float = Caml.Float (** @canonical Caml.Float *)

  module Format = Caml.Format (** @canonical Caml.Format *)

  module Fun = Caml.Fun (** @canonical Caml.Fun *)

  module Hashtbl = Caml.MoreLabels.Hashtbl (** @canonical Caml.MoreLabels.Hashtbl *)

  module Int32 = Caml.Int32 (** @canonical Caml.Int32 *)

  module Int = Caml.Int (** @canonical Caml.Int *)

  module Int64 = Caml.Int64 (** @canonical Caml.Int64 *)

  module Lazy = Caml.Lazy (** @canonical Caml.Lazy *)

  module Lexing = Caml.Lexing (** @canonical Caml.Lexing *)

  module List = Caml.StdLabels.List (** @canonical Caml.StdLabels.List *)

  module Map = Caml.MoreLabels.Map (** @canonical Caml.MoreLabels.Map *)

  module Nativeint = Caml.Nativeint (** @canonical Caml.Nativeint *)

  module Obj = Caml.Obj (** @canonical Caml.Obj *)

  module Option = Caml.Option (** @canonical Caml.Option *)

  module Parsing = Caml.Parsing (** @canonical Caml.Parsing *)

  module Printexc = Caml.Printexc (** @canonical Caml.Printexc *)

  module Printf = Caml.Printf (** @canonical Caml.Printf *)

  module Queue = Caml.Queue (** @canonical Caml.Queue *)

  module Random = Caml.Random (** @canonical Caml.Random *)

  module Result = Caml.Result (** @canonical Caml.Result *)

  module Scanf = Caml.Scanf (** @canonical Caml.Scanf *)

  module Seq = Caml.Seq (** @canonical Caml.Seq *)

  module Set = Caml.MoreLabels.Set (** @canonical Caml.MoreLabels.Set *)

  module Stack = Caml.Stack (** @canonical Caml.Stack *)

  module Stream = Caml.Stream (** @canonical Caml.Stream *)

  module String = Caml.StdLabels.String (** @canonical Caml.StdLabels.String *)

  module Sys = Caml.Sys (** @canonical Caml.Sys *)

  module Uchar = Caml.Uchar (** @canonical Caml.Uchar *)

  module Unit = Caml.Unit (** @canonical Caml.Unit *)

  include Pervasives [@ocaml.warning "-3"]

  exception Not_found = Caml.Not_found
end

external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

(* These need to be declared as an external to get the lazy behavior *)
external ( && ) : bool -> bool -> bool = "%sequand"
external ( || ) : bool -> bool -> bool = "%sequor"
external not : bool -> bool = "%boolnot"

(* We use [Obj.magic] here as other implementations generate a conditional jump and the
   performance difference is noticeable. *)
let bool_to_int (x : bool) : int = Caml.Obj.magic x

(* This need to be declared as an external for the warnings to work properly *)
external ignore : _ -> unit = "%ignore"

external ( != ) : 'a -> 'a -> bool = "%noteq"
external ( * ) : int -> int -> int = "%mulint"
external ( ** ) : float -> float -> float = "pow" [@@bs.val] [@@bs.scope "Math"]
external ( *. ) : float -> float -> float = "%mulfloat"
external ( + ) : int -> int -> int = "%addint"
external ( +. ) : float -> float -> float = "%addfloat"
external ( - ) : int -> int -> int = "%subint"
external ( -. ) : float -> float -> float = "%subfloat"
external ( / ) : int -> int -> int = "%divint"
external ( /. ) : float -> float -> float = "%divfloat"

module Poly = Poly0 (** @canonical Base.Poly *)

module Int_replace_polymorphic_compare = struct
  external ( < ) : int -> int -> bool = "%lessthan"
  external ( <= ) : int -> int -> bool = "%lessequal"
  external ( <> ) : int -> int -> bool = "%noteq"
  external ( = ) : int -> int -> bool = "%eq"
  external ( > ) : int -> int -> bool = "%greaterthan"
  external ( >= ) : int -> int -> bool = "%greaterequal"
  external compare  int -> int -> int = "%compare"
  external ascending : int -> int -> int = "%compare"
  let descending (x : int) y = compare y x
  external equal : int -> int -> bool = "%eq"
  external max : int -> int -> int = "max" [@@bs.val] [@@bs.scope "Math"]
  external min : int -> int -> int = "min" [@@bs.val] [@@bs.scope "Math"]
end

include Int_replace_polymorphic_compare

module Int32_replace_polymorphic_compare = struct
  external ( < ) : int32 -> int32 -> bool = "%lessthan"
  external ( <= ) : int32 -> int32 -> bool = "%lessequal"
  external ( <> ) : int32 -> int32 -> bool = "%noteq"
  external ( = ) : int32 -> int32 -> bool = "%eq"
  external ( > ) : int32 -> int32 -> bool = "%greaterthan"
  external ( >= ) : int32 -> int32 -> bool = "%greaterequal"
  external compare : int32 -> int32 -> int = "%compare"
  external ascending : int32 -> int32 -> int = "%compare"
  let descending (x : Caml.Int32.t) y = Poly.descending x y
  external equal : int32 -> int32 -> bool = "%eq"
  external max : int32 -> int32 -> int = "max" [@@bs.val] [@@bs.scope "Math"]
  external min : int32 -> int32 -> int = "min" [@@bs.val] [@@bs.scope "Math"]
end

module Int64_replace_polymorphic_compare = struct
  let ( < ) (x : Caml.Int64.t) y = Poly.( < ) x y
  let ( <= ) (x : Caml.Int64.t) y = Poly.( <= ) x y
  let ( <> ) (x : Caml.Int64.t) y = Poly.( <> ) x y
  let ( = ) (x : Caml.Int64.t) y = Poly.( = ) x y
  let ( > ) (x : Caml.Int64.t) y = Poly.( > ) x y
  let ( >= ) (x : Caml.Int64.t) y = Poly.( >= ) x y
  let ascending (x : Caml.Int64.t) y = Poly.ascending x y
  let descending (x : Caml.Int64.t) y = Poly.descending x y
  let compare (x : Caml.Int64.t) y = Poly.compare x y
  let equal (x : Caml.Int64.t) y = Poly.equal x y
  let max (x : Caml.Int64.t) y = if x >= y then x else y
  let min (x : Caml.Int64.t) y = if x <= y then x else y
end

module Nativeint_replace_polymorphic_compare = struct
  external ( < ) : Caml.Nativeint.t -> Caml.Nativeint.t -> bool = "%lessthan"
  external ( <= ) : Caml.Nativeint.t -> Caml.Nativeint.t -> bool = "%lessequal"
  external ( <> ) : Caml.Nativeint.t -> Caml.Nativeint.t -> bool = "%noteq"
  external ( = ) : Caml.Nativeint.t -> Caml.Nativeint.t -> bool = "%eq"
  external ( > ) : Caml.Nativeint.t -> Caml.Nativeint.t -> bool = "%greaterthan"
  external ( >= ) : Caml.Nativeint.t -> Caml.Nativeint.t -> bool = "%greaterequal"
  let ascending (x : Caml.Nativeint.t) y = Poly.ascending x y
  let descending (x : Caml.Nativeint.t) y = Poly.descending x y
  let compare (x : Caml.Nativeint.t) y = Poly.compare x y
  let equal (x : Caml.Nativeint.t) y = Poly.equal x y
  let max (x : Caml.Nativeint.t) y = if x >= y then x else y
  let min (x : Caml.Nativeint.t) y = if x <= y then x else y
end

module Bool_replace_polymorphic_compare = struct
  external ( < ) : bool -> bool -> bool = "%lessthan"
  external ( <= ) : bool -> bool -> bool = "%lessequal"
  external ( <> ) : bool -> bool -> bool = "%noteq"
  external ( = ) : bool -> bool -> bool = "%eq"
  external ( > ) : bool -> bool -> bool = "%greaterthan"
  external ( >= ) : bool -> bool -> bool = "%greaterequal"
  external ascending : bool -> bool -> int = "%compare"
  let descending (x : bool) y = Poly.descending x y
  external compare : bool -> bool -> int = "%compare"
  external equal : bool -> bool -> bool = "%equal"
  let max (x : bool) y = if x >= y then x else y
  let min (x : bool) y = if x <= y then x else y
end

module Char_replace_polymorphic_compare = struct
  let ( < ) (x : char) y = Poly.( < ) x y
  let ( <= ) (x : char) y = Poly.( <= ) x y
  let ( <> ) (x : char) y = Poly.( <> ) x y
  let ( = ) (x : char) y = Poly.( = ) x y
  let ( > ) (x : char) y = Poly.( > ) x y
  let ( >= ) (x : char) y = Poly.( >= ) x y
  let ascending (x : char) y = Poly.ascending x y
  let descending (x : char) y = Poly.descending x y
  let compare (x : char) y = Poly.compare x y
  let equal (x : char) y = Poly.equal x y
  let max (x : char) y = if x >= y then x else y
  let min (x : char) y = if x <= y then x else y
end

module Uchar_replace_polymorphic_compare = struct

  (* Here, we bank on the fact that the JS representation of Uchar is number. *)
  external i: Caml.Uchar.t -> int = "%identity"
  external ( < ) : Caml.Uchar.t -> Caml.Uchar.t -> bool = "%lessthan"
  external ( <= ) : Caml.Uchar.t -> Caml.Uchar.t -> bool = "%lessequal"
  external ( <> ) : Caml.Uchar.t -> Caml.Uchar.t -> bool = "%noteq"
  external ( = ) : Caml.Uchar.t -> Caml.Uchar.t -> bool = "%eq"
  external ( > ) : Caml.Uchar.t -> Caml.Uchar.t -> bool = "%greaterthan"
  external ( >= ) : Caml.Uchar.t -> Caml.Uchar.t -> bool = "%greaterequal"

  let ascending (x : Caml.Uchar.t) y =
    Int_replace_polymorphic_compare.ascending (i x) (i y)
  ;;

  let descending (x : Caml.Uchar.t) y =
    Int_replace_polymorphic_compare.descending (i x) (i y)
  ;;

  let compare (x : Caml.Uchar.t) y = Int_replace_polymorphic_compare.compare (i x) (i y)
  let equal (x : Caml.Uchar.t) y = Int_replace_polymorphic_compare.equal (i x) (i y)
  let max (x : Caml.Uchar.t) y = if x >= y then x else y
  let min (x : Caml.Uchar.t) y = if x <= y then x else y
end

module Float_replace_polymorphic_compare = struct
  external ( < ) : float -> float -> bool = "%lessthan"
  external ( <= ) : float -> float -> bool = "%lessequal"
  external ( <> ) : float -> float -> bool = "%noteq"
  external ( = ) : float -> float -> bool = "%eq"
  external ( > ) : float -> float -> bool = "%greaterthan"
  external ( >= ) : float -> float -> bool = "%greaterequal"
  external ascending : float -> float -> int = "%compare"
  let descending (x : float) y = Poly.descending x y
  external compare : float -> float -> int = "%compare"
  external equal : 'a -> 'a -> bool = "%eq"
  let max (x : float) y = if x >= y then x else y
  let min (x : float) y = if x <= y then x else y
end

module String_replace_polymorphic_compare = struct
  external ( < ) : string -> string -> bool = "%lessthan"
  external ( <= ) : string -> string -> bool = "%lessequal"
  external ( <> ) : string -> string -> bool = "%noteq"
  external ( = ) : string -> string -> bool = "%eq"
  external ( > ) : string -> string -> bool = "%greaterthan"
  external ( >= ) : string -> string -> bool = "%greaterequal"
  let ascending (x : string) y = Poly.ascending x y
  let descending (x : string) y = Poly.descending x y
  let compare (x : string) y = Poly.compare x y
  let equal (x : string) y = Poly.equal x y
  let max (x : string) y = if x >= y then x else y
  let min (x : string) y = if x <= y then x else y
end

module Bytes_replace_polymorphic_compare = struct

  let ( < ) (x : bytes) y = Poly.( < ) x y
  let ( <= ) (x : bytes) y = Poly.( <= ) x y
  let ( <> ) (x : bytes) y = not (Poly.( x = y ))
  let ( = ) (x : bytes) y = Poly.( = ) x y
  let ( > ) (x : bytes) y = Poly.( > ) x y
  let ( >= ) (x : bytes) y = Poly.( >= ) x y
  let ascending (x : bytes) y = Poly.ascending x y
  let descending (x : bytes) y = Poly.descending x y
  let compare (x : bytes) y = Poly.compare x y
  let equal (x : bytes) y = Poly.equal x y
  let max (x : bytes) y = if x >= y then x else y
  let min (x : bytes) y = if x <= y then x else y
end

(* This needs to be defined as an external so that the compiler can specialize it as a
   direct set or caml_modify *)
   external ( := ) : 'a ref -> 'a -> unit = "%bs_ref_setfield0"

   (* These need to be defined as an external otherwise the compiler won't unbox
      references *)
   external ( ! ) : 'a ref -> 'a = "%bs_ref_field0"
   external ref : 'a -> 'a ref = "%makemutable"
   
let ( @ ) = Caml.( @ )
let ( ^ ) = Caml.( ^ )
external ( ~- ) : int -> int = "%negint"
external ( ~-. ) : float -> float = "%negfloat"
external ( asr ) : int -> int -> int = "%asrint"
external ( land ) : int -> int -> int = "%andint"
let lnot = Caml.lnot
external ( lor ) : int -> int -> int = "%orint"
external ( lsl ) : int -> int -> int = "%lslint"
external ( lsr ) : int -> int -> int = "%lsrint"
external ( lxor ) : int -> int -> int = "%xorint"
external ( mod ) : int -> int -> int = "%modint"
let abs = Caml.abs
let failwith = Caml.failwith
external fst : 'a * 'b -> 'a = "%field0"
let invalid_arg = Caml.invalid_arg
external snd : 'a * 'b -> 'b = "%field1"

(* [raise] needs to be defined as an external as the compiler automatically replaces
   '%raise' by '%reraise' when appropriate. *)
external raise : exn -> _ = "%raise"

external phys_equal : 'a -> 'a -> bool = "%eq"

external incr : int ref -> unit = "%incr"
external decr : int ref -> unit = "%decr"

(* used by sexp_conv, which float0 depends on through option *)
external float_of_string : string -> float = "parseFloat" [@@bs.val]

(* [am_testing] is used in a few places to behave differently when in testing mode, such
   as in [random.ml].  [am_testing] is implemented using [Base_am_testing], a weak C/js
   primitive that returns [false], but when linking an inline-test-runner executable, is
   overridden by another primitive that returns [true]. *)
external am_testing : unit -> bool = "amTesting"
    [@@bs.module "@nasi/js-base-runtime"]
    [@@bs.scope "testing"]

let am_testing = am_testing ()

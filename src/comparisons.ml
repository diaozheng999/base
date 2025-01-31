(** Interfaces for infix comparison operators and comparison functions. *)

open! Import

(** [Infix] lists the typical infix comparison operators.  These functions are provided by
    [<M>.O] modules, i.e., modules that expose monomorphic infix comparisons over some
    [<M>.t]. *)
module type Infix = sig
  type t

  val ( >= ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

(** Optimised version of [Infix] specifically used for ReScript compilations. *)
module type Infix_external = sig
  type t
  external ( >= ) : t -> t -> bool = "%greaterequal"
  external ( <= ) : t -> t -> bool = "%lessequal"
  external ( = ) : t -> t -> bool = "%eq"
  external ( > ) : t -> t -> bool = "%greaterthan"
  external ( < ) : t -> t -> bool = "%lessthan"
  external ( <> ) : t -> t -> bool = "%noteq"
end

module type S = sig
  include Infix

  val equal : t -> t -> bool

  (** [compare t1 t2] returns 0 if [t1] is equal to [t2], a negative integer if [t1] is
      less than [t2], and a positive integer if [t1] is greater than [t2]. *)
  val compare : t -> t -> int

  val min : t -> t -> t
  val max : t -> t -> t
end

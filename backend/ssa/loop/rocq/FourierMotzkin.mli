module Affine : sig
  type t =
    { const : int;
      terms : (int * int) list
    }

  val const : t -> int

  val terms : t -> (int * int) list

  val constant : int -> t

  val var : int -> t

  val coeff : int -> t -> int

  val terms_equal : (int * int) list -> (int * int) list -> bool

  val equal : t -> t -> bool

  val add_const : t -> int -> t

  val scale : int -> t -> t

  val scale_checked : int -> t -> t option

  val add_checked : t -> t -> t option

  val add_const_checked : t -> int -> t option

  val sub_checked : t -> t -> t option
end

val coeff64 : int -> (int * int64) list -> int64

val scale_to_int64 : int64 -> Affine.t -> (int64 * (int * int64) list) option

val resolvent : int -> int -> Affine.t -> Affine.t -> Affine.t option

val eliminate_atom : int -> Affine.t list -> Affine.t list option

val eliminate_atoms : int list -> Affine.t list -> Affine.t list option

val feasible : Affine.t list -> bool

val entails : Affine.t list -> Affine.t -> bool

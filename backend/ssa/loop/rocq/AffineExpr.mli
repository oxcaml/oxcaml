open FourierMotzkin

type t =
  | Const of int
  | Atom of int
  | Add of t * t
  | Sub of t * t
  | Scale of int * t
  | Shr_atom of int * t * int
  | Lsr_atom of int * int
  | Or_atom of int * t * int

module Range : sig
  type t =
    { lo : int64;
      hi : int64
    }

  val lo : t -> int64

  val hi : t -> int64

  val full : t

  val const : int -> t

  val shr_signed : int -> t

  val shr_logical : int -> t

  val add64 : int64 -> int64 -> int64 option

  val mul64 : int64 -> int64 -> int64 option

  val add : t -> t -> t option

  val neg : t -> t option

  val sub : t -> t -> t option

  val or_mask : int -> t -> t option

  val scale : int -> t -> t option
end

val shr_range_facts : bool -> int -> int -> Affine.t list

val to_affine : t -> (Affine.t * Affine.t list) option

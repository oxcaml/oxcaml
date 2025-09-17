module type SHAPE = sig
  (* Axis sizes n_i >= 1; each axis i uses (n_i - 1) bits. *)
  val axis_sizes : int array
end

module Make (_ : SHAPE) : sig
  type t

  (* Shape info *)
  val num_axes : int

  val axis_sizes : int array

  (* val axis_bits : int array *)
  (* val axis_offsets : int array *)
  (* val total_bits : int *)

  (* Lattice constants *)
  val bot : t

  val top : t

  (* Lattice ops *)
  val join : t -> t -> t

  val meet : t -> t -> t

  val leq : t -> t -> bool

  val equal : t -> t -> bool

  val hash : t -> int

  val co_sub :
    t -> t -> t (* co-Heyting subtraction: least x s.t. a <= b \/ x *)

  (* Axis accessors *)
  val get_axis : t -> axis:int -> int

  val set_axis : t -> axis:int -> level:int -> t

  (* Encode/decode whole product *)
  val encode : levels:int array -> t

  val decode : t -> int array

val non_bot_axes : t -> int list

  (* Pretty-print; optionally name axes *)
  val pp : t -> string

  (* Compact representation, e.g., "[a,b,...]" where entries are axis levels. *)
  val to_string : t -> string
end

[@@@ocaml.warning "+a-40-41-42"]

(** Affine inequalities over interned integer atoms, with Fourier-Motzkin
    feasibility and entailment. Atoms are opaque integer identifiers a caller
    assigns to the values it does not decompose; this module is purely
    arithmetic and knows nothing about where they come from. *)

module Affine : sig
  (** [terms] maps each atom to a non-zero coefficient; [const] is the constant
      term. The value denotes the assertion [const + sum coeff*atom >= 0]. *)
  type t =
    { const : int;
      terms : (int * int) list
    }

  (** The constant affine form [c] (no atoms). *)
  val const : int -> t

  (** The affine form equal to atom [id] (coefficient 1). *)
  val var : int -> t

  val is_const : t -> bool

  (** The coefficient of atom [id] in [t] (0 if absent). *)
  val coeff : int -> t -> int

  val add_const : t -> int -> t

  val add : t -> t -> t

  (** Multiply every coefficient and the constant by [k]. *)
  val scale : int -> t -> t

  val neg : t -> t

  val sub : t -> t -> t
end

(** [feasible ineqs] is [true] iff the conjunction [{ f >= 0 | f in ineqs }] is
    satisfiable over the rationals, decided by Fourier-Motzkin elimination. *)
val feasible : Affine.t list -> bool

(** [entails facts goal] is [true] iff [{ f >= 0 | f in facts }] implies
    [goal >= 0]. Sound but incomplete over the integers (it reasons over the
    rationals, via [feasible] on the integer negation of the goal). *)
val entails : Affine.t list -> Affine.t -> bool

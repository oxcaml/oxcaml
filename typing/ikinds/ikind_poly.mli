(** Portable representation of normalized ikind polynomials.

    These mirror the structure produced by [Ldd_jkind_solver]: each polynomial
    is a lattice-valued decision diagram over a finite set of "rigid" boolean
    variables.  For cached kind information we restrict the rigid namespace to
    constructor arguments (identified by a type constructor [Path.t] and
    argument index) and formal type parameters (identified by their index in the
    declaration). *)

(** Rigid identifiers appearing in a polynomial. *)
module Rigid : sig
  type t =
    | Param of int
        (** [Param i] is the rigid variable corresponding to the [i]-th formal
            type parameter of the declaration whose polynomial we cached. Indices
            are zero-based. *)
    | Constr_arg of Path.t * int
        (** [Constr_arg (p, i)] is the rigid variable contributed by a recursive
            occurrence of constructor [p] at argument position [i]. *)
end

(** ZDD-style nodes with axis lattice payloads.  Leaves carry lattice elements;
    internal nodes branch on a rigid variable and encode the canonical
    "hi = hi - lo" form used by [Ldd]. *)
module Node : sig
  type t =
    | Leaf of Axis_lattice.t
    | Branch of
        { rigid : Rigid.t;
          lo : t;
          hi : t
        }
end

(** A normalized polynomial for a type constructor consists of a base node and
    a coefficient node for each formal parameter.  Coefficients are stored in an
    array whose length matches the constructor's arity. *)
type t =
  { base : Node.t;
    coeffs : Node.t array
  }

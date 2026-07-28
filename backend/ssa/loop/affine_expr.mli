[@@@ocaml.warning "+a-40-41-42"]

(** A small expression AST reifying the affine view of a machine-integer value,
    with pure evaluators.

    {!Affine_ssa} recognises SSA values into this AST (the only step that
    depends on SSA operation shapes); everything here is pure arithmetic over
    interned atoms, depending only on {!Fourier_motzkin}. The intended semantics
    is the obvious denotation with each [Atom] standing for an opaque integer;
    both evaluators below should agree with it. *)

module Affine = Fourier_motzkin.Affine

type t =
  | Const of int
  | Atom of int  (** an opaque value, interned by the caller *)
  | Add of t * t
  | Sub of t * t
  | Scale of int * t
  | Shr_atom of
      { atom : int;
        arg : t;
        bits : int
      }
      (** An atomized arithmetic right shift: [atom] is the interned atom
          standing for the shift's result, [arg] the shifted value. The value is
          [Atom atom]; the relation to [arg] is only expressible as the pair of
          bounds emitted by {!to_affine}. *)

(** The affine form of the expression, together with the side inequalities (each
    [f] asserting [f >= 0]) contributed by [Shr_atom] nodes:
    [2^bits * atom <= arg] and [arg <= 2^bits * atom + 2^bits - 1]. The form
    only over-approximates the expression when taken together with these facts,
    so callers must include them among their hypotheses. *)
val to_affine : t -> Affine.t * Affine.t list

(** [Some c] iff the affine form of the expression is the constant [c] (atom
    coefficients may cancel, e.g. [Sub (Atom a, Atom a)]). *)
val as_const : t -> int option

(** [coeff_of_atom a e] is the coefficient of atom [a] in [e], or [None] when
    [a] occurs in a non-affine position (inside a [Shr_atom]'s argument). When
    it returns [Some c], the denotation of [e] equals [c * a + r] where [r] does
    not depend on [a]. *)
val coeff_of_atom : int -> t -> int option

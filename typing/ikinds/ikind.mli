(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(*-------------------------------------------------------------------------*)
(* Types shared by ikind algorithms                                        *)
(*-------------------------------------------------------------------------*)

(** Rigid names are the variables that may occur in ikind formulas. *)
module Rigid_name : sig
  type unknown_id

  type t =
    | Atom of
        { constr : Path.t;
          arg_index : int
          (** [arg_index] = 0 refers to the base contribution, and subsequent
              indices refer to the coefficients of the i-th argument. This is
              a positional index, not a type-variable id. *)
        }
    | Param of int
        (** [Param id] only occurs in formulas for type constructors. Refers to
            a type-parameter of the constructor, where [id] is the
            [Types.get_id] of the type variable representing the parameter. *)
    | Unknown of unknown_id
        (** An unknown quantity with a given id. Used to model not-best in
            ikinds. This is used when we couldn't compute a precise ikind,
            e.g. for a polymorphic variant with conjunctive type --
            `Constr of (a & b & ...) *)

  (** Ordering on rigid names used in the LDD to order the nodes. *)
  val compare : t -> t -> int

  val to_string : t -> string

  val atomic : Path.t -> int -> t

  val param : int -> t

  val fresh_unknown : unit -> t
end

module Ldd : sig
  type node

  type var

  module Name : sig
    include module type of Rigid_name
  end

  val bot : node

  val const : Axis_lattice.t -> node

  val rigid : Name.t -> var

  val new_var : unit -> var

  val node_of_var : var -> node

  val join : node -> node -> node

  val meet : node -> node -> node

  val sub_subsets : node -> node -> node

  val solve_lfp : var -> node -> unit

  val enqueue_gfp : var -> node -> unit

  val solve_pending : unit -> unit

  (** [decompose_into_linear_terms ~universe n] returns a base term and a list
      of linear coefficients, one per variable in [universe]. *)
  val decompose_into_linear_terms :
    universe:var list -> node -> node * node list

  (** Empty list means [a ⊑ b] succeeds. Non-empty list is the witness axes
      where it fails. *)
  val leq_with_reason : node -> node -> Jkind_axis.Axis.packed list

  val round_up : node -> Axis_lattice.t

  val is_const : node -> bool

  val map_rigid : (Name.t -> node) -> node -> node

  val pp : node -> string

  val pp_debug : node -> string
end

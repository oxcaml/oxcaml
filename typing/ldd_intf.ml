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

module type Ordered = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

(** Lattice polynomial terms built from joins, meets, constants, and
    variables. The interface supports least and greatest fixpoint solving
    over these terms. *)
module type S = sig
  type node

  type var

  module Name : Ordered

  (** Constructors. *)
  val bot : node

  val const : Axis_lattice.t -> node

  val rigid : Name.t -> var

  val new_var : unit -> var

  val node_of_var : var -> node

  (** Boolean algebra over nodes. *)
  val join : node -> node -> node

  val meet : node -> node -> node

  (** [sub_subsets a b] computes co-Heyting subtraction (a - b) for LDDs. *)
  val sub_subsets : node -> node -> node

  (** Solving interface. *)
  val solve_lfp : var -> node -> unit

  val enqueue_gfp : var -> node -> unit

  val solve_pending : unit -> unit

  (** [decompose_into_linear_terms ~universe n] returns a base term and a list
      of linear coefficients, one per variable in [universe]. *)
  val decompose_into_linear_terms :
    universe:var list -> node -> node * node list

  (** If [a ⊑ b] fails, return witness axes where they differ.
      Empty list means [a ⊑ b] succeeds. Non-empty list is the witness axes
      where it fails. *)
  val leq_with_reason :
    node -> node -> Jkind_axis.Axis.packed list

  val round_up : node -> Axis_lattice.t

  val is_const : node -> bool

  val map_rigid : (Name.t -> node) -> node -> node

  (** Pretty printers and checks. *)
  val pp : node -> string

  val pp_debug : node -> string
end

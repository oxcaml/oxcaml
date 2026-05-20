(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type closure_entry_point =
  | Unknown_arity_code_pointer
      (** Entry point used for an [Indirect_unknown_arity] call. *)
  | Known_arity_code_pointer
      (** Entry point used for a [Direct] or [Indirect_known_arity] call. *)

type return_kind =
  | Normal of int (* n-th return value, for unboxed calls *)
  | Exn

type view = private
  | Block of int * Flambda_kind.t
  | Value_slot of Value_slot.t
  | Function_slot of Function_slot.t
  | Call_witness of closure_entry_point
  | Is_int
  | Get_tag
  | Return_of_call of return_kind
  | Code_id_of_call_witness

type t

include Datalog.Column.S with type t := t

val view : t -> view

(** {1 Constructing fields} *)

(** {2 Real fields}

    These are fields that "exist at runtime" in the sense that they can be
    accessed through primitives. *)

(* CR bclement: [block] is confusing, because it actually refers to a specific
   field of the block. Maybe we could call this [block_field] or [obj_field]
   instead? *)
val block : int -> Flambda_kind.t -> t

val value_slot : Value_slot.t -> t

val function_slot : Function_slot.t -> t

val is_int : t

val get_tag : t

(** {2 Virtual fields}

    These fields cannot be accessed through primitives and are used internally
    by the reaper to model function calls. *)

val known_arity_call_witness : t

val unknown_arity_call_witness : t

val call_witness : closure_entry_point -> t

val normal_return_of_call : int -> t

val exn_return_of_call : t

val code_id_of_call_witness : t

(** {1 Inspecting fields} *)

val kind : t -> Flambda_kind.t

(** [is_virtual_field f] returns [true] if [f] is a virtual field. *)
val is_virtual_field : t -> bool

(** [is_real_field f] returns [true] if [f] is a real field, i.e. not a virtual
    field. *)
val is_real_field : t -> bool

val is_value_slot : t -> bool

val is_function_slot : t -> bool

(** @raise Misc.Fatal_error if the field is not a function slot. *)
val must_be_function_slot : t -> Function_slot.t

(* CR bclement: Should this be called [is_local_slot] instead, to make it clear
   that this relates to function/value slots? *)
val is_local : t -> bool

val print_for_variable_name : Format.formatter -> t -> unit

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
  | Known_arity_code_pointer

type return_kind =
  | Normal of int
  | Exn

type view = private
  | Block of int * Flambda_kind.t
  | Value_slot of Value_slot.t
  | Function_slot of Function_slot.t
  | Code_of_closure of closure_entry_point
  | Is_int
  | Get_tag
  | Apply of return_kind
  | Code_id_of_call_witness

type t

val view : t -> view

val block : int -> Flambda_kind.t -> t

val value_slot : Value_slot.t -> t

val function_slot : Function_slot.t -> t

val code_of_closure : closure_entry_point -> t

val is_int : t

val get_tag : t

val apply : return_kind -> t

val code_id_of_call_witness : t

include Datalog.Column.S with type t := t

val kind : t -> Flambda_kind.t

val is_value_slot : t -> bool

val is_function_slot : t -> bool

val must_be_function_slot : t -> Function_slot.t

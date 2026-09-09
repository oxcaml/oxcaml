(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t

include Expr_std.S with type t := t

include Contains_ids.S with type t := t

val print_with_extra_fields :
  (Format.formatter -> unit) -> Format.formatter -> t -> unit

val is_empty : t -> bool

(** Create a set of closures given the code for its functions and the closure
    variables. *)
val create :
  is_specialisation_site:bool ->
  synthetic_value_slots:Simple.t Value_slot.Map.t ->
  value_slots:Simple.t Value_slot.Map.t ->
  Function_declarations.t ->
  t

(** A specialisation site is a closed set of closures left behind when the
    reaper lambda-lifts the functions of a set (see
    [Rebuild.rebuild_specialisation_site]). Its synthetic value slots record
    what the original value slots held, so that the lifted code can be
    specialised on those values when the enclosing code is simplified again,
    typically after inlining into another compilation unit.

    A site is heap allocated and closed, hence statically allocated. Its closure
    allocation, headers and slots contribute no cost; only its live function
    bodies contribute to cost metrics. [To_cmm] omits its data unless something
    refers to it; its synthetic value slots keep nothing alive. Inside code, it
    is never lifted and is kept while its code is live, even without synthetic
    slots: re-simplifying its functions may specialise their callees. Runtime
    uses keep its closures like any other set. Like any set of closures, it does
    prevent specialisation of the enclosing continuation handler (see
    [Specialization_cost]). *)
val is_specialisation_site : t -> bool

(** The function declarations associated with the set of closures. *)
val function_decls : t -> Function_declarations.t

(** The values of each value slot (the environment, or captured variables). *)
val value_slots : t -> Simple.t Value_slot.Map.t

(** Value slots that are recorded but not allocated: the parameters annotated
    with them (see [Function_params_and_body]) are known to equal their
    contents. The functions must not project them. Dropping a synthetic value
    slot is always sound. *)
val synthetic_value_slots : t -> Simple.t Value_slot.Map.t

(** Returns true iff the given set of closures has no value slots (synthetic
    value slots are ignored). *)
val is_closed : t -> bool

val filter_function_declarations :
  t ->
  f:
    (Function_slot.t ->
    Function_declarations.code_id_in_function_declaration ->
    bool) ->
  t

include Container_types.S with type t := t

val with_value_slots :
  t ->
  value_slots:Simple.t Value_slot.Map.t ->
  synthetic_value_slots:Simple.t Value_slot.Map.t ->
  t

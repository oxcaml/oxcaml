(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Jack Rickard, Jane Street Europe                 *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Derivation of flambda2 slot identities for layout-polymorphism environment
    sets (see [Lambda.Pset_of_closures]). The same slots are reconstructed
    deterministically in every compilation unit that mentions the template. *)

(** The (fake, [Deleted]) function slot of a template's environment set. *)
val function_slot : Lambda.template_ref -> Function_slot.t

(** The value slots of a template's environment set: one inner list per captured
    value (of the corresponding layout in [layouts]), one element per unarized
    component of that capture. Void components contribute nothing. *)
val value_slots :
  Lambda.template_ref ->
  layouts:Lambda.layout list ->
  machine_width:Target_system.Machine_width.t ->
  (Value_slot.t * Flambda_kind.With_subkind.t) list list

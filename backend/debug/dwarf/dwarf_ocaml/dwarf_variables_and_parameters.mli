(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Handling of DWARF descriptions of variables and function parameters. *)

open! Dwarf_low
open! Dwarf_high

val normal_type_for_var :
  ?reference:Proto_die.reference ->
  parent:Proto_die.t option ->
  (Compilation_unit.t * Ident.t) option ->
  Is_parameter.t ->
  Proto_die.t

(** A table assigning DIE references, in advance, to every variable in scope in
    a given function (including such variables bound by inlined frames). The
    table must be shared between the [dwarf] calls for the function's own DIE
    and for all of its inlined frames' DIEs, so that phantom variables in one
    frame can reference the DIEs of variables in another (e.g. a parameter of an
    inlined-out function whose value is a variable of the enclosing function).
*)
type proto_dies_for_vars

val build_proto_dies_for_vars :
  Available_ranges_all_vars.t -> proto_dies_for_vars

(** Which variables a given call to [dwarf] should emit DIEs for. Variables are
    claimed by the inlined frames first; whatever remains (in particular all
    variables not bound inside any inlined frame) is emitted under the
    function's own DIE, by a final call using [All_remaining_vars]. This final
    call must always be made, to ensure that none of the references in
    [proto_dies_for_vars] dangle. *)
type which_vars =
  | Vars_for_inlined_frame of Debuginfo.t
      (** The frame path of the inlined frame, as per the keys of
          [Inlined_frame_ranges]. *)
  | All_remaining_vars

val dwarf :
  Dwarf_state.t ->
  value_type_proto_die:Proto_die.t ->
  function_symbol:Asm_targets.Asm_symbol.t ->
  function_proto_die:Proto_die.t ->
  proto_dies_for_vars:proto_dies_for_vars ->
  which_vars:which_vars ->
  fun_end_label:Asm_targets.Asm_label.t ->
  Available_ranges_all_vars.t ->
  unit

(** Emit the rvalue DIEs (location lists describing the values, rather than the
    locations, of variables; consulted via [DW_OP_call*] from the location
    descriptions of phantom variables) that were demanded during the [dwarf]
    calls for the function and its inlined frames. This must be called exactly
    once per function, after all such [dwarf] calls. *)
val dwarf_rvalue_dies :
  Dwarf_state.t ->
  value_type_proto_die:Proto_die.t ->
  function_symbol:Asm_targets.Asm_symbol.t ->
  function_proto_die:Proto_die.t ->
  proto_dies_for_vars:proto_dies_for_vars ->
  fun_end_label:Asm_targets.Asm_label.t ->
  Available_ranges_all_vars.t ->
  unit

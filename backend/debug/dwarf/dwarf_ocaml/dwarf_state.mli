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

(** State that is shared amongst the various dwarf_* modules. *)

open Asm_targets
open Dwarf_low
open Dwarf_high

type function_range =
  { start_label : Asm_label.t;
    end_label : Asm_label.t;
    function_symbol : Asm_symbol.t
  }

type code_layout =
  | Continuous_code_section of
      { code_begin : Asm_symbol.t;
        code_end : Asm_symbol.t
      }
  | Function_sections

module Diagnostics : sig
  type variable_reduction =
    { shape_size_before_reduction_in_bytes : int;
      shape_size_after_reduction_in_bytes : int;
      shape_size_after_evaluation_in_bytes : int;
      reduction_steps : int;
      evaluation_steps : int;
      type_name : string;
      type_layout : Jkind_types.Sort.Const.t;
      dwarf_die_size : int;
      cms_files_loaded : int;
      cms_files_cached : int;
      cms_files_missing : string list;
      cms_files_unreadable : string list
    }

  type t = { mutable variables : variable_reduction list }
end

type t

val create :
  compilation_unit_header_label:Asm_label.t ->
  compilation_unit_proto_die:Proto_die.t ->
  value_type_proto_die:Proto_die.t ->
  code_layout:code_layout ->
  Debug_loc_table.t ->
  Debug_ranges_table.t ->
  Address_table.t ->
  Location_list_table.t ->
  get_file_num:(string -> int) ->
  sourcefile:string ->
  t

val compilation_unit_header_label : t -> Asm_label.t

val compilation_unit_proto_die : t -> Proto_die.t

val value_type_proto_die : t -> Proto_die.t

val debug_loc_table : t -> Debug_loc_table.t

val debug_ranges_table : t -> Debug_ranges_table.t

val address_table : t -> Address_table.t

val location_list_table : t -> Location_list_table.t

val function_abstract_instances :
  t -> (Proto_die.t * Asm_symbol.t) Asm_symbol.Tbl.t

val can_reference_dies_across_units : t -> bool

val get_file_num : t -> string -> int

val sourcefile : t -> string

val diagnostics : t -> Diagnostics.t

val add_variable_reduction_diagnostic :
  t -> Diagnostics.variable_reduction -> unit

val code_layout : t -> code_layout

val function_ranges : t -> function_range list

val record_function_range :
  t ->
  function_symbol:Asm_symbol.t ->
  start_label:Asm_label.t ->
  end_label:Asm_label.t ->
  unit

module Debug : sig
  val log : ('a, Format.formatter, unit) format -> 'a
end

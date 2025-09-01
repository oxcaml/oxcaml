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

open! Int_replace_polymorphic_compare
open Asm_targets
open Dwarf_low
open Dwarf_high

module Diagnostics = struct
  type variable_reduction =
    { initial_size_memory : int;
      reduced_size_memory : int;
      evaluated_size_memory : int;
      initial_size : int;
      reduced_size : int;
      evaluated_size : int;
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

type t =
  { compilation_unit_header_label : Asm_label.t;
    compilation_unit_proto_die : Proto_die.t;
    value_type_proto_die : Proto_die.t;
    start_of_code_symbol : Asm_symbol.t;
    debug_loc_table : Debug_loc_table.t;
    debug_ranges_table : Debug_ranges_table.t;
    address_table : Address_table.t;
    location_list_table : Location_list_table.t;
    function_abstract_instances : (Proto_die.t * Asm_symbol.t) Asm_symbol.Tbl.t;
    get_file_num : string -> int;
    sourcefile : string;
    diagnostics : Diagnostics.t
  }

let create ~compilation_unit_header_label ~compilation_unit_proto_die
    ~value_type_proto_die ~start_of_code_symbol debug_loc_table
    debug_ranges_table address_table location_list_table ~get_file_num
    ~sourcefile =
  { compilation_unit_header_label;
    compilation_unit_proto_die;
    value_type_proto_die;
    start_of_code_symbol;
    debug_loc_table;
    debug_ranges_table;
    address_table;
    location_list_table;
    function_abstract_instances = Asm_symbol.Tbl.create 42;
    get_file_num;
    sourcefile;
    diagnostics = { variables = [] }
  }

let compilation_unit_header_label t = t.compilation_unit_header_label

let compilation_unit_proto_die t = t.compilation_unit_proto_die

let value_type_proto_die t = t.value_type_proto_die

let start_of_code_symbol t = t.start_of_code_symbol

let debug_loc_table t = t.debug_loc_table

let debug_ranges_table t = t.debug_ranges_table

let address_table t = t.address_table

let location_list_table t = t.location_list_table

let function_abstract_instances t = t.function_abstract_instances

let can_reference_dies_across_units _t = true

let get_file_num t filename = t.get_file_num filename

let sourcefile t = t.sourcefile

let diagnostics t = t.diagnostics

let add_variable_reduction_diagnostic t diagnostic =
  t.diagnostics.variables <- diagnostic :: t.diagnostics.variables

module Debug = struct
  let log f =
    match Sys.getenv "DWARF_DEBUG" with
    | exception Not_found -> Format.ifprintf Format.err_formatter f
    | _ -> Format.eprintf f
end

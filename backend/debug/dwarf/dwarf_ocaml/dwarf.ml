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

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
open Asm_targets
open Dwarf_high
open Dwarf_low
module DAH = Dwarf_attribute_helpers
module DS = Dwarf_state
module L = Linear

module Json = struct
  (* Note that the encoding of strings into JSON strings is not perfect. We are
     using OCaml's %S format specifier, which escapes most special characters
     reasonably, but there could be some discrepancies around, for example,
     unicode characters. *)
  let field name value = Printf.sprintf "%S: %s" name value

  let string value = Printf.sprintf "%S" value

  let int value = string_of_int value

  let object_ fields =
    let field_strings = String.concat ",\n" fields in
    Printf.sprintf "{\n%s\n}" field_strings

  let array items =
    let item_strings = String.concat ",\n" items in
    Printf.sprintf "[\n%s\n]" item_strings
end
(* CR sspies: Should this go to the Misc module? It's not particularly
   efficient, but good enough for outputting JSON in this case. *)

type t =
  { state : DS.t;
    asm_directives : Asm_directives_dwarf.t;
    get_file_id : string -> int;
    mutable emitted : bool;
    mutable emitted_delayed : bool
  }

(* CR mshinwell: On OS X 10.11 (El Capitan), dwarfdump doesn't seem to be able
   to read our 64-bit DWARF output. *)

let create ~sourcefile ~unit_name ~asm_directives ~get_file_id ~code_begin
    ~code_end =
  (match !Dwarf_flags.gdwarf_format with
  | Thirty_two -> Dwarf_format.set Thirty_two
  | Sixty_four -> Dwarf_format.set Sixty_four);
  let compilation_unit_proto_die =
    Dwarf_compilation_unit.compile_unit_proto_die ~sourcefile ~unit_name
      ~code_begin ~code_end
  in
  let compilation_unit_header_label = Asm_label.create (DWARF Debug_info) in
  let value_type_proto_die =
    Proto_die.create ~parent:(Some compilation_unit_proto_die) ~tag:Base_type
      ~attribute_values:
        [ DAH.create_name "ocaml_value";
          DAH.create_encoding ~encoding:Encoding_attribute.signed;
          DAH.create_byte_size_exn ~byte_size:Arch.size_addr ]
      ()
  in
  let start_of_code_symbol =
    Cmm_helpers.make_symbol "code_begin" |> Asm_symbol.create
  in
  let debug_loc_table = Debug_loc_table.create () in
  let debug_ranges_table = Debug_ranges_table.create () in
  let address_table = Address_table.create () in
  let location_list_table = Location_list_table.create () in
  let state =
    DS.create ~compilation_unit_header_label ~compilation_unit_proto_die
      ~value_type_proto_die ~start_of_code_symbol debug_loc_table
      debug_ranges_table address_table location_list_table
      ~get_file_num:get_file_id ~sourcefile
    (* CR mshinwell: does get_file_id successfully emit .file directives for
       files we haven't seen before? *)
  in
  { state;
    asm_directives;
    emitted = false;
    emitted_delayed = false;
    get_file_id
  }

type fundecl =
  { fun_end_label : Cmm.label;
    fundecl : L.fundecl
  }

let dwarf_for_fundecl t fundecl ~fun_end_label =
  if not
       (!Clflags.debug
       && ((not !Dwarf_flags.restrict_to_upstream_dwarf)
          || !Dwarf_flags.dwarf_inlined_frames))
  then { fun_end_label; fundecl }
  else
    let available_ranges_vars, fundecl =
      if not !Dwarf_flags.restrict_to_upstream_dwarf
      then
        Profile.record "debug_available_ranges_vars"
          (fun fundecl -> Available_ranges_vars.create fundecl)
          ~accumulate:true fundecl
      else Available_ranges_vars.empty, fundecl
    in
    let inlined_frame_ranges, fundecl =
      Profile.record "debug_inlined_frame_ranges"
        (fun fundecl -> Inlined_frame_ranges.create fundecl)
        ~accumulate:true fundecl
    in
    Dwarf_concrete_instances.for_fundecl ~get_file_id:t.get_file_id t.state
      fundecl
      ~fun_end_label:(Asm_label.create_int Text (fun_end_label |> Label.to_int))
      available_ranges_vars inlined_frame_ranges;
    { fun_end_label; fundecl }

let format_variable_json (variable : DS.Diagnostics.variable_reduction) =
  let layout =
    Format.asprintf "%a" Jkind_types.Sort.Const.format variable.type_layout
  in
  let type_with_layout = Printf.sprintf "%s @ %s" variable.type_name layout in
  Json.object_
    [ Json.field "type" (Json.string type_with_layout);
      Json.field "initial_size_memory" (Json.int variable.initial_size_memory);
      Json.field "reduced_size_memory" (Json.int variable.reduced_size_memory);
      Json.field "evaluated_size_memory"
        (Json.int variable.evaluated_size_memory);
      Json.field "initial_size" (Json.int variable.initial_size);
      Json.field "reduced_size" (Json.int variable.reduced_size);
      Json.field "evaluated_size" (Json.int variable.evaluated_size);
      Json.field "reduction_steps" (Json.int variable.reduction_steps);
      Json.field "evaluation_steps" (Json.int variable.evaluation_steps);
      Json.field "dwarf_die_size" (Json.int variable.dwarf_die_size);
      Json.field "cms_files_loaded" (Json.int variable.cms_files_loaded);
      Json.field "cms_files_cached" (Json.int variable.cms_files_cached);
      Json.field "cms_files_missing"
        (Json.array (List.map Json.string variable.cms_files_missing));
      Json.field "cms_files_unreadable"
        (Json.array (List.map Json.string variable.cms_files_unreadable)) ]

let emit_stats_file t =
  let sourcefile = DS.sourcefile t.state in
  let stats_filename =
    let base = Filename.remove_extension sourcefile in
    base ^ ".debug-stats.json"
  in
  let diagnostics = DS.diagnostics t.state in
  let oc = open_out stats_filename in
  (* Format variables array *)
  let variable_jsons =
    List.rev_map format_variable_json diagnostics.variables
  in
  (* Create the main JSON object *)
  let main_object =
    Json.object_
      [ Json.field "compilation_parameters"
          (Json.object_
             [ Json.field "gdwarf_config_shape_reduce_depth"
                 (Json.int !Clflags.gdwarf_config_shape_reduce_depth);
               Json.field "gdwarf_config_shape_eval_depth"
                 (Json.int !Clflags.gdwarf_config_shape_eval_depth);
               Json.field "gdwarf_config_max_cms_files_per_unit"
                 (Json.int !Clflags.gdwarf_config_max_cms_files_per_unit);
               Json.field "gdwarf_config_max_cms_files_per_variable"
                 (Json.int !Clflags.gdwarf_config_max_cms_files_per_variable) ]);
        Json.field "variables" (Json.array variable_jsons) ]
  in
  Printf.fprintf oc "%s\n" main_object;
  close_out oc

let emit t ~basic_block_sections ~binary_backend_available =
  if t.emitted
  then
    Misc.fatal_error
      "Cannot call [Dwarf.emit] more than once on a given value of type \
       [Dwarf.t]";
  t.emitted <- true;
  Dwarf_world.emit ~asm_directives:t.asm_directives
    ~compilation_unit_proto_die:(DS.compilation_unit_proto_die t.state)
    ~compilation_unit_header_label:(DS.compilation_unit_header_label t.state)
    ~debug_loc_table:(DS.debug_loc_table t.state)
    ~debug_ranges_table:(DS.debug_ranges_table t.state)
    ~address_table:(DS.address_table t.state)
    ~location_list_table:(DS.location_list_table t.state)
    ~basic_block_sections ~binary_backend_available;
  if !Dwarf_flags.ddwarf_shape_reduction_diags then emit_stats_file t

let emit t ~basic_block_sections ~binary_backend_available =
  Profile.record "emit_dwarf"
    (emit ~basic_block_sections ~binary_backend_available)
    t

let emit_delayed t ~basic_block_sections ~binary_backend_available =
  if t.emitted_delayed
  then
    Misc.fatal_error
      "Cannot call [Dwarf.emit_delayed] more than once on a given value of \
       type [Dwarf.t]";
  t.emitted_delayed <- true;
  Dwarf_world.emit_delayed ~asm_directives:t.asm_directives
    ~basic_block_sections ~binary_backend_available

let emit_delayed t ~basic_block_sections ~binary_backend_available =
  Profile.record "emit_delayed_dwarf"
    (emit_delayed ~basic_block_sections ~binary_backend_available)
    t

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Mark Shinwell, Jane Street                      *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Command-line wrapper around [Debug_printers], following the protocol of
   lldb's external pretty-printer support for OxCaml (see
   OxCamlExternalPrinter.cpp in the lldb OxCaml plugin):

   - [Sys.argv.(1)] is a type name, e.g. "Types.type_expr";
   - stdin carries one value of that type in Marshal wire format;
   - if the type is recognized, the value's rendering is written to stdout
     and the exit code is 0 (lldb shows stdout as the value's summary);
   - otherwise the exit code is nonzero and lldb falls back to its
     built-in formatter.

   [Marshal.from_channel] is not type-safe: the value on stdin must really
   have the type named by [Sys.argv.(1)], and this executable must be built
   from the same compiler sources as the program being debugged. *)

let fmt = Format.std_formatter

(* Read the marshalled value from stdin and print it with [printer]. *)
let run (printer : _ Format_doc.format_printer) () =
  let v = Marshal.from_channel stdin in
  printer fmt v

(* Each entry maps the accepted spellings of a type name to the
   corresponding printer from [Debug_printers].  The first spelling is the
   canonical type path; the name of the printer itself (the value's name in
   debug_printers.ml) is also accepted.  The spelling that lldb passes
   comes from the DWARF info and may differ (it can be inspected with
   "log enable oxcaml formatting" in lldb); extend the lists as needed. *)
let entries : (string list * (unit -> unit)) list =
  [ ( [ "Types.type_expr";
        "type_expr";
        "Types.transient_expr";
        "transient_expr" ],
      run Debug_printers.type_expr );
    ( ["Btype.TypeSet.t"; "TypeSet.t"; "type_set"],
      run Debug_printers.type_set );
    ["Types.row_field"; "row_field"], run Debug_printers.row_field;
    ["Types.row_desc"; "row_desc"], run Debug_printers.row_desc;
    ["Ident.t"; "ident"], run Debug_printers.ident;
    ["Path.t"; "path"], run Debug_printers.path;
    ( ["Ctype.global_state"; "global_state"; "ctype_global_state"],
      run Debug_printers.ctype_global_state );
    ( ["Jkind.Sort.t"; "Jkind_types.Sort.t"; "Sort.t"; "sort"],
      run Debug_printers.sort );
    ( ["Jkind.Sort.var"; "Jkind_types.Sort.var"; "Sort.var"; "sort_var"],
      run Debug_printers.sort_var );
    ["Jkind.t"; "Types.jkind"; "jkind"], run Debug_printers.jkind;
    ( ["Zero_alloc.t"; "zero_alloc_var"],
      run Debug_printers.zero_alloc_var );
    ( [ "Uniqueness_analysis.Maybe_unique.t";
        "Maybe_unique.t";
        "maybe_unique" ],
      run Debug_printers.maybe_unique );
    ( [ "Uniqueness_analysis.Maybe_aliased.t";
        "Maybe_aliased.t";
        "maybe_aliased" ],
      run Debug_printers.maybe_aliased );
    ( ["Uniqueness_analysis.Aliased.t"; "Aliased.t"; "aliased"],
      run Debug_printers.aliased );
    ( ["Uniqueness_analysis.Tag.t"; "Tag.t"; "tag"],
      run Debug_printers.tag );
    ( [ "Uniqueness_analysis.Projection.t";
        "Projection.t";
        "projection" ],
      run Debug_printers.projection );
    ( [ "Uniqueness_analysis.Usage_tree.t";
        "Usage_tree.t";
        "usage_tree" ],
      run Debug_printers.usage_tree );
    ( [ "Uniqueness_analysis.Usage_forest.t";
        "Usage_forest.t";
        "usage_forest" ],
      run Debug_printers.usage_forest );
    ( ["Uniqueness_analysis.Paths.t"; "Paths.t"; "paths"],
      run Debug_printers.paths );
    ( ["Uniqueness_analysis.Value.t"; "Value.t"; "value"],
      run Debug_printers.value );
    ( ["Uniqueness_analysis.Ienv.t"; "Ienv.t"; "ienv"],
      run Debug_printers.ienv );
    ( ["Jkind_axis.Axis_set.t"; "Axis_set.t"; "axis_set"],
      run Debug_printers.axis_set );
    ( ["Jkind.Layout.t"; "Jkind_types.Layout.t"; "Layout.t"; "layout"],
      run Debug_printers.layout );
    ( [ "Btype.Jkind0.Mod_bounds.t";
        "Jkind0.Mod_bounds.t";
        "Mod_bounds.t";
        "mod_bounds" ],
      run Debug_printers.mod_bounds );
    ( ["Jkind.With_bounds.t"; "Types.with_bounds"; "with_bounds"],
      run Debug_printers.with_bounds );
    ( [ "Types.with_bounds_types";
        "with_bounds_types" ],
      run Debug_printers.with_bounds_types );
    ( ["Mode.Modality.Const.t"; "Modality.Const.t"; "modalities"],
      run Debug_printers.modalities );
    ( ["Compilation_unit.t"; "compilation_unit"],
      run Debug_printers.compilation_unit );
    ( [ "Compilation_unit.Name.t";
        "Compilation_unit.Name";
        "compilation_unit_name" ],
      run Debug_printers.compilation_unit_name );
    ( ["Import_info.t"; "import_info"],
      run Debug_printers.import_info ) ]

(* Compilers with stamped names in the debug info can produce spellings
   such as "type_expr/318" or "Types.type_expr/318"; strip the stamps. *)
let strip_stamps name =
  let strip_component component =
    match String.index_opt component '/' with
    | None -> component
    | Some i -> String.sub component 0 i
  in
  String.concat "."
    (List.map strip_component (String.split_on_char '.' name))

let list_supported () =
  List.iter
    (fun (names, _) -> print_endline (String.concat " " names))
    entries

let usage () =
  prerr_endline
    "usage: debug_printers_wrapper TYPE-NAME < MARSHALLED-VALUE\n\
     \       debug_printers_wrapper -list\n\
     Prints on stdout the value read from stdin, which must be in Marshal\n\
     wire format and have the given type; exits nonzero if the type name\n\
     is not recognized.  -list shows the accepted type names."

let () =
  if Array.length Sys.argv <> 2 then begin usage (); exit 2 end;
  match Sys.argv.(1) with
  | "-list" | "--list" -> list_supported ()
  | requested_name -> (
    let requested = strip_stamps requested_name in
    match
      List.find_opt (fun (names, _) -> List.mem requested names) entries
    with
    | None ->
      Printf.eprintf "unknown type name %S\n" requested_name;
      exit 1
    | Some (_, print) ->
      set_binary_mode_in stdin true;
      (try print ()
       with exn ->
         Format.pp_print_flush fmt ();
         Printf.eprintf "error printing value of type %S: %s\n"
           requested_name (Printexc.to_string exn);
         exit 3);
      Format.pp_print_flush fmt ())

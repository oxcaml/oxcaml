(******************************************************************************
 *                                 Chamelon                                   *
 *                         Milla Valnet, OCamlPro                             *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2023 OCamlPro                                                *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** Minimizer **)

open Utils
open Cmt_format
open Chamelon_args

(* ______ COMMAND SETUP ______ *)

let all_minimizers =
  List.fold_left
    (fun minimizers m -> Smap.add m.minimizer_name m minimizers)
    Smap.empty
    [ Deletelines.minimizer;
      Flatteningmodules.minimizer;
      Inlinefunction.minimizer;
      Inlinenever.minimizer;
      Reducedef.minimizer;
      Reduceexpr.minimizer;
      Reduceexpr.minimizer_dummy1;
      (* Reduceexpr_typesafe.minimizer; *)
      Remdef.minimizer;
      Removeattributes.minimizer;
      Removeconsfields.minimizer;
      Removedeadcode.minimizer;
      Removeunit.minimizer;
      Removeunusedargs.minimizer;
      Removeunusedrec.minimizer;
      Sequentializefunctions.minimizer;
      Simplifyapplication.minimizer;
      Simplifymatch.minimizer;
      Simplifysequences.minimizer;
      Simplifytypes.minimizer;
      Reducepat.minimizer;
      Stub.minimizer;
      Cleanup.minimizer ]

let default_iteration =
  [ "stub";
    "delete-lines";
    "reduce-expr";
    "reduce-expr-2";
    "remove-dead-code";
    "reduce-pat";
    "inline-never";
    "remove-unit";
    "reduce-def";
    "remove-dead-code";
    "simplify-sequences";
    "remove-unused-args";
    "remove-unused-rec";
    "sequentialize-functions";
    "simplify-sequences";
    "sequentialize-functions";
    "inline-function";
    "simplify-application";
    "simplify-match";
    "simplify-application";
    "simplify-match";
    "flatten-modules";
    (* "remove-attributes"; *)
    "simplify-types";
    "remove-cons-fields" ]

(* CHECKING ERROR PRESENCE AFTER PRINTING *)

type mode =
  | Legacy_multi_files of
      { command : string;
        input_files : string list;
        output_dir : string
      }
  | Modules of
      { modname : string;
        command : string;
        output_dir : string;
        context : Context.t
      }

let main () =
  let minimizers_to_run =
    let minimizer_names =
      if !arg_minimizers = ""
      then default_iteration
      else String.split_on_char ',' !arg_minimizers
    in
    let to_exclude =
      if !exclude_minimizers = ""
      then []
      else String.split_on_char ',' !exclude_minimizers
    in
    List.filter_map
      (fun name ->
        match Smap.find name all_minimizers with
        | minimizer -> if List.mem name to_exclude then None else Some minimizer
        | exception Not_found ->
          Format.eprintf "Minimizer %S not found@." name;
          exit 1)
      minimizer_names
  in
  let schedule =
    if !test >= 0
    then (
      if List.compare_length_with minimizers_to_run 1 <> 0
      then (
        Format.eprintf "Please provide exactly one minimizer in test mode@.";
        exit 1);
      Iterator.with_strategy
        (Iterator.test ~pos:!test ~len:1)
        [Context.minimizer (List.hd minimizers_to_run)])
    else Iterator.fix (List.map Context.minimizer minimizers_to_run)
  in
  (* LIST MINIMIZERS *)
  if !list_minimizers
  then (
    Format.printf "@[<v 2>Available minimizers:@ @[<v>%a@]@]@."
      (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf (name, _) ->
           Format.pp_print_string ppf name))
      (Smap.bindings all_minimizers);
    exit 0);
  (* PARSING COMMAND AND READING FILES*)
  if !command = ""
  then (
    Format.eprintf "No command provided (hint: `-c` argument is mandatory).@.";
    Arg.usage run_spec_list usage_msg;
    exit 2);
  let file_names = List.rev !input_files in
  let cmt_infos =
    if !cmt_files = []
    then
      let cmt_command =
        if !typing_command = "" then !command else !typing_command
      in
      generate_cmt cmt_command file_names
    else if !typing_command = ""
    then List.rev_map read_cmt !cmt_files
    else (
      Format.eprintf "Options --cmt and -t are incompatible.@.";
      exit 2)
  in
  let cmt_files_with_names = List.combine file_names cmt_infos in
  (* CHECKING ERROR PRESENCE *)
  let c =
    if !inplace
    then !command
    else List.fold_left (fun c output -> c ^ " " ^ output) !command file_names
  in
  if !test < 0 && not (raise_error c)
  then (
    Format.eprintf "This command does not raise the error %S. @."
      !Utils.error_str;
    exit 1);
  let mode =
    match !inplace, cmt_files_with_names with
    | false, [(input_file, cmt_info)] ->
      let sourcefile =
        if !output_file = ""
        then
          let ext = Filename.extension input_file in
          let stem = Filename.chop_extension input_file in
          stem ^ "_min" ^ ext
        else !output_file
      in
      let { modname; context } : Context.add_result =
        Context.add ~sourcefile cmt_info Context.empty
      in
      let command = !command ^ " " ^ sourcefile in
      Modules { modname; command; output_dir = ""; context }
    | true, [(input_file, cmt_info)] ->
      if !output_file != ""
      then (
        Format.eprintf "Options -i and -o are not compatible@.";
        exit 2);
      let { modname; context } : Context.add_result =
        Context.add ~sourcefile:input_file cmt_info Context.empty
      in
      Modules { modname; command = !command; output_dir = ""; context }
    | false, _ ->
      if !output_file = ""
      then (
        Format.eprintf
          "Multi-file minimization requires either --inplace or -o.";
        exit 2);
      let output_dir = !output_file in
      Legacy_multi_files
        { command = !command; input_files = file_names; output_dir }
    | true, (first_file, first_cmt) :: other_inputs ->
      let { modname; context } : Context.add_result =
        Context.add ~sourcefile:first_file first_cmt Context.empty
      in
      let context =
        List.fold_left
          (fun context (sourcefile, cmt_file) ->
            let add_result = Context.add ~sourcefile cmt_file context in
            add_result.context)
          context other_inputs
      in
      Modules { modname; command = !command; output_dir = ""; context }
    | true, [] ->
      Format.eprintf "In-place minimization requires at least one input.@.";
      exit 2
  in
  let check ~path ~command modules =
    Context.write_to ~path modules;
    if raise_error command
    then (
      Context.write_to ~path
        ~with_open_out:(fun name f ->
          Out_channel.with_open_bin (name ^ ".tmp") f)
        modules;
      true)
    else false
  in
  match mode with
  | Legacy_multi_files { command; input_files; output_dir } ->
    Format.eprintf "Running in legacy multi-file mode.@.";
    ignore
      (Sys.command (Filename.quote_command "cp" ["-R"; "."; output_dir ^ "/"]));
    Sys.chdir output_dir;
    (* MINIMIZING FILES *)
    let rfile_names = ref file_names in
    let context =
      List.fold_left
        (fun context (sourcefile, cmt_file) ->
          let add_result = Context.add ~sourcefile cmt_file context in
          add_result.context)
        Context.empty cmt_files_with_names
    in
    let structures = Context.structures context in
    let file_strs =
      List.map
        (fun input_file ->
          match Smap.find_opt input_file structures with
          | None -> failwith "missing structure"
          | Some structure -> structure)
        input_files
    in
    let rfile_strs = ref file_strs in
    let str_map =
      Context.of_modules
        (List.fold_left2
           (fun map key str -> Smap.add key str map)
           Smap.empty file_names file_strs)
    in
    let c = ref (make_command command input_files) in
    let nmap = ref str_map in
    Context.write_to ~path:output_dir !nmap;
    if !test < 0 then Utils.must_raise_error !c;
    let has_changed = ref true in
    while !has_changed do
      (* REMOVING FILES *)
      let modules =
        List.fold_left2
          (fun modules name str -> Smap.add name str modules)
          Smap.empty !rfile_names !rfile_strs
      in
      let fn, fs = Removefiles.to_remove command (!rfile_names, !rfile_strs) in
      let fs =
        List.map
          (fun modinfo ->
            match modinfo.implementation with
            | Some { annots; _ } -> annots
            | None -> assert false)
          fs
      in
      let fn, fs = Mergefiles.merge_strategy command (fn, fs) in
      let fs =
        List.map2
          (fun name str ->
            let modinfo = Smap.find name modules in
            update_structure modinfo str)
          fn fs
      in
      rfile_names := fn;
      rfile_strs := fs;
      nmap
        := Context.of_modules
             (List.fold_left2
                (fun map key str -> Smap.add key str map)
                Smap.empty fn fs);
      c := make_command command fn;
      let a, b =
        List.fold_left
          (fun (map, b) name ->
            let nmap, ch =
              Iterator.run
                ~check:(check ~path:output_dir ~command:!c)
                schedule map name
            in
            nmap, b || ch)
          (!nmap, false) fn
      in
      nmap := a;
      has_changed := b
    done
  | Modules { modname; command; output_dir; context } ->
    Context.write_to ~path:output_dir context;
    must_raise_error command;
    let context, _did_change =
      Iterator.run
        ~check:(check ~path:output_dir ~command)
        schedule context modname
    in
    if !test < 0 then Context.write_to ~path:output_dir context

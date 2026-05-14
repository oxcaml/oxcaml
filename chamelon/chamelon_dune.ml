(******************************************************************************
 *                                 Chamelon                                   *
 *                         Basile Clément, OCamlPro                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 OCamlPro                                                *
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

[@@@warning "-32-69"]

open Minios
module Sexp = Minisexp

type file_dep =
  | In_source_tree of string
  | In_build_dir of string
  | External of string

type glob_dep =
  { dir : file_dep;
    predicate : string
  }

type dep =
  | File of file_dep
  | Glob of glob_dep

type targets =
  { files : string list;
    directories : string list
  }

type action =
  | Copy of
      { src : string;
        dst : string
      }
  | Symlink of
      { src : string;
        dst : string
      }
  | Chdir of string * action
  | Ext of string
  | Echo of string
  | Run of Cmd.t
  | With_stdout_to of string * action
  | Write_file of
      { path : string;
        contents : string
      }
  | Bash of string
  | Progn of action list
  | Other of string * Sexp.t list

type rule =
  { deps : dep list;
    targets : targets;
    context : string;
    action : action
  }

(******************************************************************************)
(* Conversions from S-expressions                                             *)
(******************************************************************************)

module Of_sexp = struct
  open Sexp

  let file_dep =
    variant_of_sexp
      [ ( "In_source_tree",
          let+ path = one_arg string_of_sexp in
          In_source_tree path );
        ( "In_build_dir",
          let+ path = one_arg string_of_sexp in
          In_build_dir path );
        ( "External",
          let+ path = one_arg string_of_sexp in
          External path ) ]

  let glob_dep =
    let+ dir, (predicate, (_only_generated_files, ())) =
      Fields.parse
        [ "dir", file_dep;
          "predicate", string_of_sexp;
          "only_generated_files", bool_of_sexp ]
    in
    { dir; predicate }

  let dep =
    variant_of_sexp
      [ ( "File",
          let+ file_dep = one_arg file_dep in
          File file_dep );
        ( "glob",
          let+ glob_dep = one_arg glob_dep in
          Glob glob_dep ) ]

  let deps = list_of_sexp dep

  let targets =
    let+ files, (directories, ()) =
      Fields.parse
        [ "files", list_of_sexp string_of_sexp;
          "directories", list_of_sexp string_of_sexp ]
    in
    { files; directories }

  let copy_action =
    let+ src, dst = pair string_of_sexp string_of_sexp in
    Copy { src; dst }

  let ext_action =
    let+ ext = one_arg string_of_sexp in
    Ext ext

  let echo_action =
    let+ arg = one_arg string_of_sexp in
    Echo arg

  let write_file_action =
    let+ path, contents = pair string_of_sexp string_of_sexp in
    Write_file { path; contents }

  let run_action sexps =
    let argv = List.map string_of_sexp sexps in
    Run (Cmd.of_list argv)

  let bash_action =
    let+ cmd = one_arg string_of_sexp in
    Bash cmd

  let symlink_action =
    let+ src, dst = pair string_of_sexp string_of_sexp in
    Symlink { src; dst }

  let other_action name sexps =
    Format.eprintf "unknown action: %s@." name;
    Other (name, sexps)

  let rec action sexp =
    variant_of_sexp ~default:other_action
      [ "copy", copy_action;
        "symlink", symlink_action;
        "chdir", chdir_action;
        "ext", ext_action;
        "echo", echo_action;
        "run", run_action;
        "with-stdout-to", with_stdout_to_action;
        "write-file", write_file_action;
        "bash", bash_action;
        "progn", progn_action ]
      sexp

  and chdir_action sexp =
    let dir, action = pair string_of_sexp action sexp in
    Chdir (dir, action)

  and with_stdout_to_action sexp =
    let stdout, action = pair string_of_sexp action sexp in
    With_stdout_to (stdout, action)

  and progn_action sexps =
    let actions = List.map action sexps in
    Progn actions

  let rule =
    let+ deps, (targets, (context, (action, ()))) =
      Fields.parse
        [ "deps", deps;
          "targets", targets;
          "context", string_of_sexp;
          "action", action ]
    in
    { deps; targets; context; action }
end

let rule_of_sexp = Of_sexp.rule

(******************************************************************************)
(* Conversions to S-expressions                                               *)
(******************************************************************************)

let sexp_of_list sexp_of lst = Sexp.List (List.map sexp_of lst)

let sexp_of_string str = Sexp.Atom str

let sexp_of_constructor name args = Sexp.List (Sexp.Atom name :: args)

let sexp_of_named_tuple lst =
  Sexp.List
    (List.map (fun (name, sexp) -> Sexp.List [Sexp.Atom name; sexp]) lst)

let sexp_of_file_dep = function
  | In_source_tree name ->
    sexp_of_constructor "In_source_tree" [sexp_of_string name]
  | In_build_dir name -> sexp_of_constructor "In_build_dir" [sexp_of_string name]
  | External name -> sexp_of_constructor "External" [sexp_of_string name]

let sexp_of_glob_dep { dir; predicate } =
  sexp_of_named_tuple
    ["dir", sexp_of_file_dep dir; "predicate", sexp_of_string predicate]

let sexp_of_dep = function
  | File file_dep -> sexp_of_constructor "File" [sexp_of_file_dep file_dep]
  | Glob glob_dep -> sexp_of_constructor "glob" [sexp_of_glob_dep glob_dep]

let sexp_of_targets { files; directories } =
  sexp_of_named_tuple
    [ "files", sexp_of_list sexp_of_string files;
      "directories", sexp_of_list sexp_of_string directories ]

let sexp_of_rule { deps; targets; context; action = _ } =
  sexp_of_named_tuple
    [ "deps", sexp_of_list sexp_of_dep deps;
      "targets", sexp_of_targets targets;
      "context", sexp_of_string context ]

(******************************************************************************)
(* Printers in shell format                                                   *)
(******************************************************************************)

let pp_print_shell_space ppf () =
  Format.pp_print_custom_break ppf ~fits:("", 1, "") ~breaks:(" \\", 0, "")

let pp_print_shell_newline ppf () =
  Format.pp_print_custom_break ppf ~fits:(";", 1, "") ~breaks:("", 0, "")

let rec quote_action ppf = function
  | Chdir (path, action) ->
    Format.fprintf ppf "(@[<hv>%s &&%a%a@])"
      (Filename.quote_command "cd" [path])
      pp_print_shell_newline () quote_action action
  | Run cmd -> Format.fprintf ppf "%s" (Cmd.to_string cmd)
  | Copy { src; dst } ->
    Format.fprintf ppf "%s" (Filename.quote_command "cp" ["-f"; src; dst])
  | Symlink { src; dst } ->
    Format.fprintf ppf "%s" (Filename.quote_command "ln" ["-sf"; dst; src])
  | Echo str -> Format.fprintf ppf "%s" (Filename.quote_command "echo" [str])
  | Ext ext -> Format.fprintf ppf "dune-extension %s" (Filename.quote ext)
  | Other (name, _) ->
    invalid_arg (Format.asprintf "quote_action: other (%s)" name)
  | With_stdout_to (path, action) ->
    Format.fprintf ppf "(%a >%s)" quote_action action (Filename.quote path)
  | Write_file { path; contents } ->
    Format.fprintf ppf "%s"
      (Filename.quote_command ~stdout:path "echo" [contents])
  | Bash cmd -> Format.fprintf ppf "%s" cmd
  | Progn [] -> Format.fprintf ppf "true"
  | Progn [action] -> quote_action ppf action
  | Progn actions ->
    Format.fprintf ppf "@[<hov 2>(%a)@]"
      (Format.pp_print_list ~pp_sep:pp_print_shell_newline quote_action)
      actions

let quote_rule ppf { targets; action; _ } =
  (* Make sure to remove targets so that we don't accidentally reuse stale files
     if they are not properly generated by running the action.

     Also, this happens to fix permission issues with the files created by
     `dune` itself.*)
  (match targets.files with
  | [] -> ()
  | files ->
    Format.fprintf ppf "@[<hov 2>rm -f%a%a@]@ " pp_print_shell_space ()
      (Format.pp_print_list Format.pp_print_string ~pp_sep:pp_print_shell_space)
      (List.map Filename.quote files));
  quote_action ppf action

let rec map_run_arguments f = function
  | Chdir (path, action) -> Chdir (path, map_run_arguments f action)
  | With_stdout_to (stdout, action) ->
    With_stdout_to (stdout, map_run_arguments f action)
  | Run args -> Run (f args)
  | Progn actions -> Progn (List.map (map_run_arguments f) actions)
  | (Copy _ | Symlink _ | Ext _ | Echo _ | Write_file _ | Bash _ | Other _) as
    action ->
    action

let rec relative_to = function
  | Chdir (path, action) -> Filename.concat path (relative_to action)
  | With_stdout_to (_stdout, action) -> relative_to action
  | Run _ | Copy _ | Symlink _ | Ext _ | Echo _ | Write_file _ | Bash _ -> ""
  | Progn _ | Other _ -> failwith "not a relative action"

(******************************************************************************)
(* Dependency computation                                                     *)
(******************************************************************************)

module Graph : sig
  type t

  val create : unit -> t

  val add : t -> rule -> unit

  val rule_for : t -> target:string -> rule

  val rules_for : t -> targets:string list -> rule list

  val dependencies : t -> string list -> rule list

  val reverse_dependencies : t -> string list -> rule list
end = struct
  type t =
    { rules : rule Dynarray.t;
      rule_for_target : (string, int) Hashtbl.t;
      (* Map from target name to the unique rule producing that target. *)
      target_dependencies : (string, int) Hashtbl.t
          (* Map from target name to all the rules that depend on that target.
             Uses [Hashtbl.find_all]. *)
    }

  let create () =
    { rules = Dynarray.create ();
      rule_for_target = Hashtbl.create 17;
      target_dependencies = Hashtbl.create 17
    }

  let rule_id_for_target graph target =
    try Hashtbl.find graph.rule_for_target target
    with Not_found ->
      Format.ksprintf failwith "Unable to find rule producing: %s" target

  let rule_for graph ~target =
    Dynarray.get graph.rules (rule_id_for_target graph target)

  let rules_for graph ~targets =
    let rule_ids = List.map (rule_id_for_target graph) targets in
    let unique_rules = List.sort_uniq Int.compare rule_ids in
    List.map (Dynarray.get graph.rules) unique_rules

  let add { rules; rule_for_target; target_dependencies } rule =
    (* Forcefully add `-warn-error -A -w -a` to all `ocamlc` and `ocamlopt`
       invocations; the minimized code produced by chamelon typically has a
       bunch of warnings. *)
    let action =
      map_run_arguments
        (fun cmd ->
          let tool = Cmd.get_line_tool cmd in
          if
            List.mem
              (Filename.remove_extension (Filename.basename tool))
              ["ocamlc"; "ocamlopt"]
          then Cmd.(cmd % "-warn-error" % "-A" % "-w" % "-a")
          else cmd)
        rule.action
    in
    let rule = { rule with action } in
    let rule_id = Dynarray.length rules in
    Dynarray.add_last rules rule;
    (* Record the rule that produces a given target. *)
    List.iter
      (fun target ->
        assert (not (Hashtbl.mem rule_for_target target));
        Hashtbl.replace rule_for_target target rule_id)
      rule.targets.files;
    (* Record the dependencies for files in the build directory. *)
    List.iter
      (function
        | File (In_build_dir target) ->
          if not (Hashtbl.mem rule_for_target target)
          then Format.eprintf "WARNING: Rules not added in topological order.@.";
          Hashtbl.add (* not replace *) target_dependencies target rule_id
        | File (In_source_tree _ | External _) | Glob _ -> ())
      rule.deps

  let traversal graph ~f ~init =
    let processed_rules = Dynarray.make (Dynarray.length graph.rules) false in
    let to_process = Queue.create () in
    let mark rule_id =
      if not (Dynarray.get processed_rules rule_id)
      then (
        Dynarray.set processed_rules rule_id true;
        Queue.push rule_id to_process)
    in
    let rec process_rules () =
      match Queue.pop to_process with
      | exception Queue.Empty -> ()
      | rule_id ->
        assert (Dynarray.get processed_rules rule_id);
        let rule = Dynarray.get graph.rules rule_id in
        f ~mark rule;
        process_rules ()
    in
    List.iter (fun target -> mark (rule_id_for_target graph target)) init;
    process_rules ();
    (* Slightly inefficient since this is always O(rules), but likely fine. *)
    let rules = ref [] in
    Dynarray.iteri
      (fun rule_id must_include ->
        if must_include then rules := Dynarray.get graph.rules rule_id :: !rules)
      processed_rules;
    List.rev !rules

  let dependencies graph targets =
    let process_rule ~mark rule =
      List.iter
        (function
          | Glob _ | File (In_source_tree _) | File (External _) -> ()
          | File (In_build_dir target) -> mark (rule_id_for_target graph target))
        rule.deps
    in
    traversal graph ~f:process_rule ~init:targets

  let reverse_dependencies graph targets =
    let process_rule ~mark rule =
      List.iter
        (fun target ->
          let rev_deps = Hashtbl.find_all graph.target_dependencies target in
          List.iter mark rev_deps)
        rule.targets.files
    in
    traversal graph ~f:process_rule ~init:targets
end

type minimization_target =
  { cmt_rules : rule list;
    rules : rule list
  }

let compute_minimization_targets graph failing_targets =
  let deps = Graph.dependencies graph failing_targets in
  (* Compute the rules generating .cmx files (that are not in an "install"
     context, since those rules are merely copies). *)
  let cmx_rules =
    List.filter_map
      (fun ({ targets; context; _ } as rule) ->
        if context = "install"
        then None
        else
          match
            List.find_opt
              (fun name -> Filename.extension name = ".cmx")
              targets.files
          with
          | None -> None
          | Some cmx_target -> Some (cmx_target, rule))
      deps
  in
  let cmx_and_cmi_targets, minimization_sets =
    List.fold_left
      (fun (current_set, all_sets) (cmx_target, cmx_rule) ->
        (* Find the corresponding .cmi based on the dependencies of the
           .cmx-generating rule with the same unit name. *)
        assert (Filename.extension cmx_target = ".cmx");
        let current_set = cmx_target :: current_set in
        let cmx_prefix =
          Filename.basename (Filename.chop_extension cmx_target)
        in
        let cmi_suffix = cmx_prefix ^ ".cmi" in
        let cmi_target =
          List.find_map
            (function
              | File (In_build_dir path)
                when Filename.extension path = ".cmi"
                     && Filename.basename path = cmi_suffix ->
                Some path
              | File _ | Glob _ -> None)
            cmx_rule.deps
        in
        let current_set =
          match cmi_target with
          | None -> current_set
          | Some cmi_target -> cmi_target :: current_set
        in
        let cmt_targets =
          match cmi_target with
          | None -> [cmx_target]
          | Some cmi_target -> [cmi_target; cmx_target]
        in
        let cmt_rules = Graph.rules_for graph ~targets:cmt_targets in
        let rules = Graph.reverse_dependencies graph current_set in
        let all_sets = { cmt_rules; rules } :: all_sets in
        current_set, all_sets)
      ([], []) (List.rev cmx_rules)
  in
  List.rev minimization_sets, cmx_and_cmi_targets

let remove_targets rule =
  List.iter
    (fun path -> if Sys.file_exists path then Sys.remove path)
    rule.targets.files

let shell = "/bin/sh"

let generate_cmt_files ~dune_extra_args { targets; context; action; _ } modnames
    cmt_context =
  let expected_cmt_files =
    List.filter_map
      (fun target ->
        match Filename.extension target with
        | ".cmx" -> Some (Filename.remove_extension target ^ ".cmt")
        | ".cmi" -> Some (Filename.remove_extension target ^ ".cmti")
        | _ -> None)
      targets.files
  in
  match expected_cmt_files with
  | [] -> modnames, cmt_context
  | _ :: _ -> (
    (* Remove the expected files to make sure we don't use stale ones (e.g. if
       the compilation command somehow fails). *)
    List.iter
      (fun path -> if Sys.file_exists path then Sys.remove path)
      expected_cmt_files;
    (* Generate the .cmt or .cmti files *)
    let cmt_action =
      map_run_arguments
        (fun cmd -> Cmd.(cmd % "-bin-annot" % "-stop-after" % "typing"))
        action
    in
    let cmt_command = Format.asprintf "%a" quote_action cmt_action in
    (* Wrap the cmt-generating command in `dune exec` in order to ensure the
       environment is correctly set up. *)
    let cmt_cmd =
      Cmd.(
        v "dune" % "exec" %% dune_extra_args % "--context" % context % "--"
        % shell % "-c" % cmt_command)
    in
    match eval_cmd_with_capture cmt_cmd with
    | Error stdout ->
      Format.eprintf "@[<v 1>@[<hv 2>%a@]@ %s@]@." Format.pp_print_text
        "Failed to generate .cmt(i):" stdout;
      exit 1
    | Ok _stdout ->
      let root_dir = relative_to cmt_action in
      List.fold_left
        (fun (modnames, cmt_context) cmt_target ->
          if Sys.file_exists cmt_target
          then
            let cmt_infos = Cmt_format.read_cmt cmt_target in
            let sourcefile =
              match cmt_infos.cmt_sourcefile with
              | None -> assert false
              | Some sourcefile -> Filename.concat root_dir sourcefile
            in
            let { Chamelon_lib.Context.modname; context = cmt_context } =
              Chamelon_lib.Context.add ~sourcefile cmt_infos cmt_context
            in
            let modnames = modname :: modnames in
            modnames, cmt_context
          else if
            (* If the file does not exist but we were expecting a .cmti file,
               this might be because we are compiling an implementation without
               interface, in which case a .cmt for the implementation is
               generated instead. Skip it: we will use the .cmt file generated
               by the compilation of the .cmx for that file. *)
            Filename.extension cmt_target = ".cmti"
          then modnames, cmt_context
          else (
            Format.eprintf "@[<v 1>@[<hv 2>%a@]@ %s@]@." Format.pp_print_text
              "Compilation rule did not generate expected .cmt file:" cmt_target;
            exit 1))
        (modnames, cmt_context) expected_cmt_files)

let print_temp_file prefix suffix pp args =
  let filename, channel = Filename.open_temp_file prefix suffix in
  match
    let ppf = Format.formatter_of_out_channel channel in
    pp ppf args;
    Format.pp_print_flush ppf ()
  with
  | () ->
    close_out channel;
    filename
  | exception exn ->
    close_out_noerr channel;
    raise exn

let print_build_script ppf rules =
  Format.fprintf ppf "@[<v>#!/bin/sh@ @ ";
  Format.fprintf ppf "@[<h>set -euo pipefail@]@ @ ";
  Format.pp_print_list quote_rule ppf rules;
  Format.fprintf ppf "@]"

let named name =
  Chamelon_lib.Context.minimizer
    (Chamelon_lib.Utils.Smap.find name Chamelon_run.all_minimizers)

let default_schedule =
  let open Chamelon_lib.Iterator in
  list
    [ fix (List.map named Chamelon_run.default_iteration);
      fix [named "cleanup"; named "delete-lines"] ]

let schedule_for_failing_target = default_schedule

let schedule_for_other_targets =
  (* For targets outside of the main target, first try to aggressively remove as
     much of the file as possible using the "stub" and "delete-lines"
     minimizers. *)
  Chamelon_lib.Iterator.(
    list
      [ fix [with_strategy eager (List.map named ["stub"; "delete-lines"])];
        default_schedule ])

let minimize_target ~dune_extra_args ~schedule context { cmt_rules; rules } =
  (* CR bclement: Should we run the command once to ensure that dependencies of
     the [cmt_rules] are available? *)
  let modnames, context =
    List.fold_right
      (fun rule (modnames, cmt_context) ->
        remove_targets rule;
        generate_cmt_files ~dune_extra_args rule modnames cmt_context)
      (List.rev cmt_rules) ([], context)
  in
  let modnames = List.sort_uniq String.compare modnames in
  let modname =
    match modnames with
    | [modname] -> modname
    | [] | _ :: _ :: _ ->
      Format.eprintf "Expected a single module name for minimization target@.";
      exit 1
  in
  let build_sh_name = print_temp_file "build" ".sh" print_build_script rules in
  let with_open_out path f = Out_channel.with_open_bin path f in
  let command = Filename.quote_command shell [build_sh_name] in
  let check modules =
    Chamelon_lib.Context.write_to ~with_open_out ~path:"" modules;
    Chamelon_lib.Utils.raise_error command
  in
  Format.eprintf "Running chamelon on: %s@." build_sh_name;
  Format.eprintf "Minimizing module: %s@." modname;
  if not (Chamelon_lib.Utils.raise_error command)
  then (
    Format.eprintf "This command does not raise the error %s. @."
      !Utils.error_str;
    exit 1);
  Chamelon_lib.Context.write_to ~with_open_out ~path:"" context;
  Utils.must_raise_error command;
  let context, _has_changed =
    Chamelon_lib.Iterator.run ~check schedule context modname
  in
  Chamelon_lib.Context.write_to ~with_open_out ~path:"" context;
  context

let extract_failing_target stderr =
  (* Parse output of dune's `--debug-dependency-path`.

     This yields lines of the form "-> required by <target>", with a twist: the
     <target> may be on a different line if it is long. *)
  let marker = "-> required by" in
  let marker_len = String.length marker in
  let rec find_marker_in_lines = function
    | [] -> None
    | line :: rest ->
      let trimmed = String.trim line in
      if String.starts_with ~prefix:marker trimmed
      then
        let after_marker =
          String.sub trimmed marker_len (String.length trimmed - marker_len)
        in
        let target = String.trim after_marker in
        if String.equal "" target
        then
          match rest with
          | [] -> None
          | target :: _ -> Some (String.trim target)
        else Some target
      else find_marker_in_lines rest
  in
  let lines = String.split_on_char '\n' stderr in
  match find_marker_in_lines lines with
  | Some target -> target
  | None -> failwith "Could not find failing target in dune output"

(* Command line options parsing *)

open Chamelon_args

let add_dune_extra_args cmd =
  let cmd = if !root = "" then cmd else Cmd.(cmd % "--root" % !root) in
  let cmd =
    if !workspace = "" then cmd else Cmd.(cmd % "--workspace" % !workspace)
  in
  cmd

let add_dune_build_extra_args cmd =
  match !only_packages with
  | None -> cmd
  | Some pkgs -> Cmd.(cmd % "--only-packages" % pkgs)

let parse_rules_for_target ~dune_extra_args ~extra_args_for_failing_targets
    failing_targets =
  let graph = Graph.create () in
  let cmd =
    Cmd.(
      v "dune" % "show" % "rules" %% dune_extra_args % "-r"
      %% of_list failing_targets)
  in
  match eval_cmd_with_capture cmd with
  | Ok stdout ->
    let sexps = Minisexp.from_string stdout in
    List.iter
      (fun sexp ->
        let rule = rule_of_sexp sexp in
        let rule =
          if
            List.exists
              (fun target -> List.mem target failing_targets)
              rule.targets.files
          then
            let action =
              map_run_arguments
                Cmd.(fun cmd -> cmd %% extra_args_for_failing_targets)
                rule.action
            in
            { rule with action }
          else rule
        in
        Graph.add graph rule)
      sexps;
    graph
  | Error stdout ->
    Format.eprintf "@[<v>%a:@ %s@]@." Format.pp_print_text
      "Could not parse the output of `dune show rules`" stdout;
    exit 1

let infer_failing_target targets =
  let dune_build_cmd =
    Cmd.(v "dune" % "build" %% of_list targets)
    |> add_dune_extra_args |> add_dune_build_extra_args
  in
  let dune_build_cmd =
    if !Chamelon_args.assume_failing
    then dune_build_cmd
    else Cmd.(dune_build_cmd % "--stop-on-first-error")
  in
  (* Make sure to build at least once. *)
  match eval_cmd dune_build_cmd with
  | Ok () -> if !Chamelon_args.assume_failing then targets else exit 0
  | Error () ->
    if !Chamelon_args.assume_failing
    then targets
    else (
      (* Extract failing target from dune's dependency path output *)
      Format.eprintf
        "Re-running dune compilation with --debug-dependency-path@.";
      let dune_build_cmd = Cmd.(dune_build_cmd % "--debug-dependency-path") in
      match eval_cmd_with_capture dune_build_cmd with
      | Ok _stdout ->
        Format.eprintf "Build succeeded, nothing to minimize.";
        exit 1
      | Error stderr ->
        let failing_target = extract_failing_target stderr in
        [failing_target])

let main () =
  let targets = List.rev !targets in
  let dune_extra_args = add_dune_extra_args Cmd.empty in
  let failing_targets = infer_failing_target targets in
  Format.eprintf "Identified failing targets: %a@."
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_string)
    failing_targets;
  let extra_args_for_failing_targets =
    Cmd.of_list (List.rev !Chamelon_args.extra_args_for_failing_target)
  in
  let graph =
    parse_rules_for_target ~dune_extra_args ~extra_args_for_failing_targets
      failing_targets
  in
  let minimization_targets, cmx_and_cmi_targets =
    compute_minimization_targets graph failing_targets
  in
  let failing_target, other_targets =
    match minimization_targets with
    | [] -> assert false
    | failing_target :: other_targets -> failing_target, other_targets
  in
  let context =
    minimize_target ~dune_extra_args ~schedule:schedule_for_failing_target
      Chamelon_lib.Context.empty failing_target
  in
  let context =
    List.fold_left
      (minimize_target ~dune_extra_args ~schedule:schedule_for_other_targets)
      context other_targets
  in
  Chamelon_lib.Context.write_to ~path:"" context;
  (* Regenerate *)
  let cmx_and_cmi_rules = Graph.rules_for graph ~targets:cmx_and_cmi_targets in
  let all_rules = Graph.reverse_dependencies graph cmx_and_cmi_targets in
  let build_sh_name =
    print_temp_file "build" ".sh" print_build_script all_rules
  in
  Format.eprintf
    "================================================================================@.";
  Format.eprintf "Minimization complete!@.";
  Format.eprintf "@[<v 2>@[Compilation script for failing build:@]@ %s@]@."
    build_sh_name;
  Format.eprintf
    "================================================================================@.";
  let command = Filename.quote_command shell [build_sh_name] in
  assert (Chamelon_lib.Utils.raise_error command);
  let _modnames, context =
    List.fold_right
      (fun rule (modnames, cmt_context) ->
        remove_targets rule;
        generate_cmt_files ~dune_extra_args rule modnames cmt_context)
      (List.rev cmx_and_cmi_rules)
      ([], Chamelon_lib.Context.empty)
  in
  Format.eprintf "%a@." Chamelon_lib.Context.print context

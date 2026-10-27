(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Helper functions when writing actions *)

open Ocamltest_stdlib

let fail_with_reason reason =
  let code _log env =
    let result = Result.fail_with_reason reason in
    (Actions.Static result, env)
  in
  Actions.make ~name:"fail" ~description:"Make the test fail"
    ~does_something:true
    code

let skip_with_reason reason =
  let code _log env =
    let result = Result.skip_with_reason reason in
    (Actions.Static result, env)
  in
  Actions.make ~name:"skip" ~description:"Skip the test"
    ~does_something:true
    code

let predicate test pass_reason _skip_reason _log env =
  let open Result in
  let result =
    (* We always use the [pass_reason].  If we used the [skip_reason] in the
       failure case, there would be a double negation, and the error message
       printed would be wrong. *)
    if test
    then predicate_satisfied_with_reason pass_reason
    else predicate_not_satisfied_with_reason pass_reason in
  (Actions.Static result, env)

let mkreason what commandline exitcode =
  Printf.sprintf "%s: command\n%s\nfailed with exit code %d"
    what commandline exitcode

let testfile env =
  match Environments.lookup Builtin_variables.test_file env with
  | None -> assert false
  | Some t -> t

let test_source_directory env =
  Environments.safe_lookup Builtin_variables.test_source_directory env

let test_build_directory env =
  Environments.safe_lookup Builtin_variables.test_build_directory env

let test_build_directory_prefix env =
  Environments.safe_lookup Builtin_variables.test_build_directory_prefix env

let words_of_variable env variable =
  String.words (Environments.safe_lookup variable env)

let exit_status_of_variable env variable =
  try int_of_string
    (Environments.safe_lookup variable env)
  with _ -> 0

let readonly_files env = words_of_variable env Builtin_variables.readonly_files

let subdirectories env = words_of_variable env Builtin_variables.subdirectories

let setup_symlinks test_source_directory build_directory files =
  let symlink filename =
    (* Emulate ln -sfT *)
    let src = Filename.concat test_source_directory filename in
    let dst = Filename.concat build_directory filename in
    let () =
      if Sys.file_exists dst then
        if Sys.win32 && Sys.is_directory dst then
          (* Native symbolic links to directories don't disappear with unlink;
             doing rmdir here is technically slightly more than ln -sfT would
             do *)
          Sys.rmdir dst
        else
          Sys.remove dst
    in
      Unix.symlink src dst in
  let copy filename =
    let src = Filename.concat test_source_directory filename in
    let dst = Filename.concat build_directory filename in
    Sys.copy_file src dst in
  let f = if Unix.has_symlink () then symlink else copy in
  Sys.make_directory build_directory;
  List.iter f files

let setup_subdirectories source_directory build_directory subdirs =
  let full_src_path name = Filename.concat source_directory name in
  let full_dst_path name = Filename.concat build_directory name in
  let cp_dir name =
    Sys.copy_directory (full_src_path name) (full_dst_path name)
  in
  List.iter cp_dir subdirs

let setup_build_env add_testfile ?(remove=[]) additional_files
      (_log : out_channel) env =
  let source_dir = (test_source_directory env) in
  let build_dir = (test_build_directory env) in
  let some_files = additional_files @ (readonly_files env) in
  let files =
    if add_testfile
    then (testfile env) :: some_files
    else some_files in
  let subdirs = subdirectories env in
  Actions.Task [Setup_build_env { source_dir; build_dir;
                                  remove; files; subdirs }],
  env

let do_setup_build_env ~source_dir ~build_dir ~remove ~files ~subdirs =
  setup_symlinks source_dir build_dir files;
  setup_subdirectories source_dir build_dir subdirs;
  List.iter Sys.force_remove remove;
  Sys.chdir build_dir;
  Result.pass

let setup_simple_build_env add_testfile additional_files log env =
  let build_env = Environments.add
    Builtin_variables.test_build_directory
    (test_build_directory_prefix env) env in
  setup_build_env add_testfile additional_files log build_env

let run_cmd
    ~full_environment
    ?(environment=[||])
    ?(stdin_variable=Builtin_variables.stdin)
    ?(stdout_variable=Builtin_variables.stdout)
    ?(stderr_variable=Builtin_variables.stderr)
    ?(append=false)
    ?timeout
    ?(exits=[0,Result.pass])
    ~what
    log env original_cmd
  =
  let log_redirection std filename =
    if filename<>"" then
    begin
      Printf.fprintf log "  Redirecting %s to %s \n%!" std filename
    end in
  let cmd =
    if (Environments.lookup_as_bool Strace.strace env) = Some true then
    begin
      let action_name = Environments.safe_lookup Actions.action_name env in
      let test_build_directory = test_build_directory env in
      let strace_logfile_name = Strace.get_logfile_name action_name in
      let strace_logfile =
        Filename.make_path [test_build_directory; strace_logfile_name]
      in
      let strace_flags = Environments.safe_lookup Strace.strace_flags env in
      let strace_cmd =
        ["strace"; "-f"; "-o"; strace_logfile; strace_flags]
      in
      strace_cmd @ original_cmd
    end else original_cmd
  in
  let lst = List.concat (List.map String.words cmd) in
  let quoted_lst =
    if Sys.win32
    then List.map Filename.maybe_quote lst
    else lst in
  let cmd' = String.concat " " quoted_lst in
  Printf.fprintf log "Commandline: %s\n" cmd';
  let progname = List.hd quoted_lst in
  let arguments = Array.of_list quoted_lst in
  let stdin_filename = Environments.safe_lookup stdin_variable env in
  let stdout_filename = Environments.safe_lookup stdout_variable env in
  let stderr_filename = Environments.safe_lookup stderr_variable env in
  log_redirection "stdin" stdin_filename;
  log_redirection "stdout" stdout_filename;
  log_redirection "stderr" stderr_filename;
  let systemenv =
    Environments.append_to_system_env
      ~full:full_environment
      environment
      env
  in
  let timeout =
    match timeout with
    | Some timeout -> timeout
    | None ->
        Option.value ~default:0
          (Environments.lookup_as_int Builtin_variables.timeout env)
  in
  let settings =
    {
      Run_command.progname = progname;
      Run_command.argv = arguments;
      Run_command.envp = systemenv;
      Run_command.stdin_filename = stdin_filename;
      Run_command.stdout_filename = stdout_filename;
      Run_command.stderr_filename = stderr_filename;
      Run_command.append = append;
      Run_command.timeout = timeout;
      Run_command.log = log
    }
  in
  let message =
    Printf.sprintf "%s: command\n%s" what (String.concat " " original_cmd)
  in
  Actions.Task [Run_cmd { settings; message; exits }]

let do_run_cmd ~(settings : Run_command.settings) ~message ~exits log =
  let n = Run_command.run settings in
  let dump_file s fn =
    if not (Sys.file_is_empty fn) then begin
      Printf.fprintf log "### begin %s ###\n" s;
      Sys.dump_file log fn;
      Printf.fprintf log "### end %s ###\n" s
    end
  in
  dump_file "stdout" settings.stdout_filename;
  if settings.stdout_filename <> settings.stderr_filename then
    dump_file "stderr" settings.stderr_filename;
  try List.assoc n exits
  with Not_found ->
    let reason = Printf.sprintf "%s\nfailed with exit code %d" message n in
    Result.fail_with_reason reason

let run
    ~full_environment
    ?exits
    (log_message : string)
    (redirect_output : bool)
    (prog_variable : Variables.t)
    (args_variable : Variables.t option)
    (log : out_channel)
    (env : Environments.t)
  =
  match Environments.lookup prog_variable env with
  | None ->
    let msg = Printf.sprintf "%s: variable %s is undefined"
      log_message (Variables.name_of_variable prog_variable) in
    (Actions.Static (Result.fail_with_reason msg), env)
  | Some program ->
    let arguments = match args_variable with
      | None -> ""
      | Some variable -> Environments.safe_lookup variable env in
    let commandline = [program; arguments] in
    let what = log_message ^ " " ^ program ^ " " ^
    begin if arguments="" then "without any argument"
    else "with arguments " ^ arguments
    end in
    let stdout_variable, stderr_variable =
      if redirect_output
      then begin
        (if Environments.is_variable_defined Builtin_variables.stdout env
         then Some Builtin_variables.stdout
         else Some Builtin_variables.output),
        (if Environments.is_variable_defined Builtin_variables.stderr env
         then Some Builtin_variables.stderr
         else Some Builtin_variables.output)
      end else None, None
    in
    let expected_exit_status =
      exit_status_of_variable env Builtin_variables.exit_status, Result.pass
    in
    let exits = Option.value exits ~default:[expected_exit_status] in
    run_cmd ~full_environment ?stdout_variable ?stderr_variable
      ~exits ~what log env commandline, env

let run_cmd = run_cmd ~full_environment:false

(* Any extra OCAMLRUNPARAM components given by -run-ocamlrunparam are appended
   here, for this action only, rather than in the initial environment:
   the compilers and other tools run without them. *)
let run_program log env =
  let env =
    if Options.run_ocamlrunparam = "" then env
    else begin
      let existing =
        Environments.safe_lookup Ocaml_variables.ocamlrunparam env in
      let value =
        if existing = "" then Options.run_ocamlrunparam
        else existing ^ "," ^ Options.run_ocamlrunparam in
      Environments.add Ocaml_variables.ocamlrunparam value env
    end
  in
  run
    ~full_environment:false
    "Running program"
    true
    Builtin_variables.program
    (Some Builtin_variables.arguments)
    log env

let run_script log env =
  let response_file = "ocamltest-" ^ testfile env ^ ".response" in
  Printf.fprintf log "Script should write its response to %s\n%!"
    response_file;
  let scriptenv = Environments.add
    Builtin_variables.ocamltest_response response_file env in
  let (task, newenv) = run
    ~full_environment:true
    "Running script"
    true
    Builtin_variables.script
    None
    log scriptenv in
  Actions.seq [task; Task [Check_file_absent { filename = response_file }]],
  newenv
(*
 * Support for reading modifiers from ocamltest_response is disabled in oxcaml
 *
  let final_value =
    if Result.is_pass result then begin
      result, newenv
    end else begin
      let reason = String.trim (Sys.string_of_file response_file) in
      let newresult = { result with Result.reason = Some reason } in
      (newresult, newenv)
    end
  in
  Sys.force_remove response_file;
  final_value *)

let run_config_script log env =
  let response_file = Filename.temp_file "ocamltest" ".response" in
  let scriptenv = Environments.add
    Builtin_variables.ocamltest_response response_file env in
  let (task, newenv) = run
    ~full_environment:true
    ~exits:[0, Result.pass; 125, Result.skip]
    "Running script"
    true
    Builtin_variables.script
    None
    log scriptenv in
  (* This command is run immediately, during setup. *)
  let r =
    match[@ocaml.warning "-fragile-match"] task with
    | Task [Run_cmd {settings;message;exits}] ->
       do_run_cmd ~settings ~message ~exits log
    | _ -> assert false
  in
  let task =
    if Result.is_pass r then task
    else
      let reason = String.trim (Sys.string_of_file response_file) in
      Actions.Static { r with reason = Some reason }
  in
  Sys.remove response_file;
  task, newenv

let run_hook hook_name log input_env =
  Printf.fprintf log "Entering run_hook for hook %s\n%!" hook_name;
  let response_file = "ocamltest-" ^ testfile input_env ^ ".response" in
  Printf.fprintf log "Hook should write its response to %s\n%!"
    response_file;
  let hookenv = Environments.add
    Builtin_variables.ocamltest_response response_file input_env in
  let systemenv =
    Environments.to_system_env ~full:true hookenv in
  let timeout =
    Option.value ~default:0
      (Environments.lookup_as_int Builtin_variables.timeout input_env) in
  let open Run_command in
  let settings = {
    progname = "sh";
    argv = [|"sh"; Filename.maybe_quote hook_name|];
    envp = systemenv;
    stdin_filename = "";
    stdout_filename = "";
    stderr_filename = "";
    append = false;
    timeout = timeout;
    log = log;
  } in
  let exits = [0, Result.pass] in
  Actions.Task [
    Run_cmd { settings; message = "hook " ^ hook_name; exits };
    Check_file_absent { filename = response_file } ],
  input_env
(*
 * Support for reading modifiers from ocamltest_response is disabled in oxcaml
 *
  let final_value = match exit_status with
    | 0 ->
      Result.pass, hookenv
    | _ ->
      Printf.fprintf log "Hook returned %d" exit_status;
      let reason = String.trim (Sys.string_of_file response_file) in
      if exit_status=125
      then (Result.skip_with_reason reason, hookenv)
      else (Result.fail_with_reason reason, hookenv)
  in
  Sys.force_remove response_file;
  final_value
 *)

let compare_files kind_of_output output_filename reference_filename log
    env =
  let to_int = function None -> 0 | Some s -> int_of_string s in
  let skip_lines =
    to_int (Environments.lookup Builtin_variables.skip_header_lines env) in
  let skip_bytes =
    to_int (Environments.lookup Builtin_variables.skip_header_bytes env) in
  Printf.fprintf log "Comparing %s output %s to reference %s\n%!"
    kind_of_output output_filename reference_filename;
  let files =
  {
    Filecompare.filetype = Filecompare.Text;
    Filecompare.reference_filename = reference_filename;
    Filecompare.output_filename = output_filename
  } in
  let reference_exists = Sys.file_exists reference_filename in
  let ignore_header_conf = {
      Filecompare.lines = skip_lines;
      Filecompare.bytes = skip_bytes;
    } in
  let promote = Environments.lookup_as_bool Builtin_variables.promote env = Some true in
  let tool =
    Filecompare.make_cmp_tool ~ignore:ignore_header_conf in
  Actions.Task [Compare_files {kind_of_output; files; tool; reference_exists; promote;}], env

let do_compare_files ~kind_of_output ~files ~tool ~promote log =
  let Filecompare.{filetype=_; reference_filename; output_filename} = files in
  match Filecompare.check_file ~tool files with
    | Filecompare.Same -> Result.pass
    | Filecompare.Different ->
      let diff = Filecompare.diff files in
      let diffstr = match diff with
        | Ok difference -> difference
        | Error diff_file -> ("See " ^ diff_file) in
      let reason =
        Printf.sprintf "%s output %s differs from reference %s: \n%s\n"
        kind_of_output output_filename reference_filename diffstr in
      if promote
      then begin
        Printf.fprintf log "Promoting %s output %s to reference %s\n%!"
          kind_of_output output_filename reference_filename;
        let ignore_header_conf : Filecompare.ignore =
          match tool with
          | Filecompare.Internal ign -> ign
          | Filecompare.External _ -> { bytes = 0; lines = 0 }
        in
        Filecompare.promote files ignore_header_conf;
      end;
      Result.fail_with_reason reason
    | Filecompare.Unexpected_output ->
      let banner = String.make 40 '=' in
      let unexpected_output = Sys.string_of_file output_filename in
      let unexpected_output_with_banners = Printf.sprintf
        "%s\n%s%s\n" banner unexpected_output banner in
      let reason = Printf.sprintf
        "The file %s was expected to be empty because there is no \
          reference file %s but it is not:\n%s\n"
        output_filename reference_filename unexpected_output_with_banners in
      Result.fail_with_reason reason
    | Filecompare.Error (commandline, exitcode) ->
      let reason = Printf.sprintf "The command %s failed with status %d"
        commandline exitcode in
      Result.fail_with_reason reason

let check_output kind_of_output output_variable reference_variable log
    env =
  compare_files kind_of_output
    (Environments.safe_lookup output_variable env)
    (Environments.safe_lookup reference_variable env)
    log env

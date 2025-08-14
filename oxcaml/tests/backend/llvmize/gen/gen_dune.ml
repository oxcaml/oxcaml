(* filenames don't contain extensions *)
type task =
  | C of string
  | Ocaml_llvm of
      { filename : string;
        stop_after_llvmize : bool
      }
  | Ocaml_default of string
  | Output_ir of
      { source : string;
        output : string
      }

let rule_of_task = function
  | Ocaml_default filename ->
    Format.sprintf "(run ${ocamlopt} %s.ml -c ${common_flags})" filename
  | Ocaml_llvm { filename; stop_after_llvmize } ->
    let common_flags =
      if stop_after_llvmize
      then "${stop_after_llvm_flags}"
      else "${common_flags}"
    in
    Format.sprintf "(run ${ocamlopt} %s.ml -c ${llvm_flags} %s)" filename
      common_flags
  | Output_ir { source; output } ->
    Format.sprintf
      {|(with-outputs-to %s.output.corrected (pipe-outputs (run cat %s.ll) (run ./${filter})))|}
      output source
  | C filename -> Format.sprintf "(run clang %s.c ${c_flags})" filename

let dependency_of_task = function
  | Ocaml_llvm { filename; _ } | Ocaml_default filename ->
    Some (filename ^ ".ml")
  | C filename -> Some (filename ^ ".c")
  | Output_ir _ -> None

let target_of_task = function
  | Output_ir { output; _ } -> Some (output ^ ".output.corrected")
  | Ocaml_default filename | Ocaml_llvm { filename; stop_after_llvmize = false }
    ->
    Some (filename ^ ".cmx")
  | C filename -> Some (filename ^ ".o")
  | Ocaml_llvm { stop_after_llvmize = true; _ } -> None

let can_run = function
  | Output_ir _ -> false
  | Ocaml_default _ | Ocaml_llvm _ | C _ -> true

module F = struct
  open Format

  let pp_space ppf () = fprintf ppf " "

  let pp_newline ppf () = fprintf ppf "\n   "

  let pp_strings pp_sep = pp_print_list ~pp_sep pp_print_string

  let check_env_rule =
    let message =
      "ERROR: OXCAML_CLANG environment variable not set.\n\
       Llvmize tests require a custom LLVM build.\n\
       Please set OXCAML_CLANG to the path of your custom Clang binary.\n\
       Example: export OXCAML_CLANG=/path/to/custom/clang\n"
    in
    asprintf
      {|(rule
 ${enabled_if_without_llvm}
 (alias runtest-llvmize)
 (action
  (progn
   (echo %S)
   (bash "exit 1"))))

|}
      message

  let exe_rule ~deps ~output =
    asprintf "(run ${ocamlopt} %a -opaque -o %s.exe)" (pp_strings pp_space) deps
      output

  (* CR yusumez: Make one rule per task to better use incremental tests. *)
  let pp_compile_rule ppf ~targets ~deps ~task_rules =
    fprintf ppf
      {|(rule
 ${enabled_if}
 (targets %a)
 (deps %a)
 (action
  (progn
   %a)))

|}
      (pp_strings pp_space) targets (pp_strings pp_space) deps
      (pp_strings pp_newline) task_rules

  let pp_run_exe_rule ppf ~output =
    fprintf ppf
      {|(rule
 ${enabled_if}
 (alias runtest-llvmize)
 (deps %s.exe)
 (targets %s.corrected)
 (action
  (with-outputs-to
   %s.corrected
   (run ./%s.exe))))

|}
      output output output output

  let pp_compare_output_rule ppf ~output =
    fprintf ppf
      {|(rule
 ${enabled_if}
 (alias runtest-llvmize)
 (deps %s %s.corrected)
 (action
  (diff %s %s.corrected)))

|}
      output output output output

  let pp_rule_template ~run ~tasks ppf () =
    let run_args =
      Option.map
        (fun output ->
          ( List.filter can_run tasks |> List.filter_map target_of_task,
            output ^ ".output" ))
        run
    in
    let deps = List.filter_map dependency_of_task tasks in
    let targets =
      List.filter_map target_of_task tasks
      @ match run_args with None -> [] | Some (_, output) -> [output ^ ".exe"]
    in
    let task_rules =
      List.map rule_of_task tasks
      @
      match run_args with
      | None -> []
      | Some (deps, output) -> [exe_rule ~deps ~output]
    in
    pp_compile_rule ppf ~deps ~targets ~task_rules;
    (match run_args with
    | None -> ()
    | Some (_, output) ->
      pp_run_exe_rule ppf ~output;
      pp_compare_output_rule ppf ~output);
    List.iter
      (function
        | Output_ir { output; _ } ->
          pp_compare_output_rule ppf ~output:(output ^ ".output")
        | C _ | Ocaml_default _ | Ocaml_llvm _ -> ())
      tasks
end

let print_rule ~extra_subst ~buf rule_template =
  let enabled_if =
    {|(enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))|}
  in
  let enabled_if_without_llvm =
    {|(enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (= %{env:OXCAML_CLANG=} "")))|}
  in
  (* Prioritise [extra_subst] *)
  let subst label =
    match
      List.find_opt (fun (label', _) -> String.equal label label') extra_subst
    with
    | Some (_, res) -> res
    | None -> (
      match label with
      | "ocamlopt" -> "%{bin:ocamlopt.opt}"
      | "enabled_if" -> enabled_if
      | "enabled_if_without_llvm" -> enabled_if_without_llvm
      | "filter" -> "filter.sh"
      | "llvm_path" -> "${OXCAML_CLANG}"
      | "llvm_flags" ->
        (* We pass -dno-asm-comments to avoid printing flaky identifiers in Cfg
           instructions *)
        (* CR yusumez: remove -disable-poll-insertion once we can emit poll
           insertions *)
        (* CR yusumez: find a better way to detect LLVM_PATH *)
        "-llvm-backend -llvm-path ${OXCAML_CLANG} -keep-llvmir \
         -dno-asm-comments -disable-poll-insertion"
      | "common_flags" ->
        "-g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear"
      | "stop_after_llvm_flags" ->
        "-g -O3 -opaque -dump-into-file -dcmm -dcfg -stop-after llvmize"
      | "c_flags" -> "-c -g -O3 -I %{project_root}/runtime"
      | _ -> assert false)
  in
  Buffer.clear buf;
  Buffer.add_substitute buf subst rule_template;
  Buffer.output_buffer Out_channel.stdout buf

let print_test ~extra_subst ~run ~tasks ~buf =
  let rule_template =
    Format.asprintf "%a" (F.pp_rule_template ~run ~tasks) ()
  in
  print_rule ~extra_subst ~buf rule_template

let ocaml_llvm_and_output_ir name =
  [ Ocaml_llvm { filename = name; stop_after_llvmize = false };
    Output_ir { source = name; output = name ^ "_ir" } ]

let () =
  let buf = Buffer.create 1000 in
  let print_test_ir_only name =
    print_test ~extra_subst:[] ~buf ~run:None
      ~tasks:
        [ Ocaml_llvm { filename = name; stop_after_llvmize = true };
          Output_ir { source = name; output = name } ]
  in
  let print_test_ir_and_run name =
    let main_name = name ^ "_main" in
    print_test ~extra_subst:[] ~buf ~run:(Some name)
      ~tasks:(ocaml_llvm_and_output_ir name @ [Ocaml_default main_name])
  in
  let print_test_ir_and_run_with_dep ~extra_dep_suffix
      ?(extra_dep_with_llvm_backend = false) name =
    let extra_dep_name = name ^ "_" ^ extra_dep_suffix in
    let main_name = name ^ "_main" in
    print_test ~extra_subst:[] ~buf ~run:(Some name)
      ~tasks:
        ([ (if extra_dep_with_llvm_backend
           then
             Ocaml_llvm
               { filename = extra_dep_name; stop_after_llvmize = false }
           else Ocaml_default extra_dep_name) ]
        @ ocaml_llvm_and_output_ir name
        @ [Ocaml_default main_name])
  in
  let print_test_c ~c_suffix name =
    let c_name = name ^ "_" ^ c_suffix in
    let main_name = name ^ "_main" in
    print_test ~extra_subst:[] ~buf ~run:(Some name)
      ~tasks:
        [ C c_name;
          Ocaml_llvm { filename = name; stop_after_llvmize = false };
          Output_ir { source = name; output = name ^ "_ir" };
          Ocaml_default main_name ]
  in
  let print_test_run_no_main name =
    print_test ~extra_subst:[] ~buf ~run:(Some name)
      ~tasks:(ocaml_llvm_and_output_ir name)
  in
  print_rule ~extra_subst:[] ~buf F.check_env_rule;
  print_test_ir_only "id_fn";
  print_test_ir_and_run "const_val";
  print_test ~extra_subst:[] ~buf ~run:(Some "int_ops")
    ~tasks:
      ([C "int_ops_stub"; Ocaml_default "int_ops_data"]
      @ ocaml_llvm_and_output_ir "int_ops"
      @ [Ocaml_default "int_ops_main"]);
  print_test_ir_and_run_with_dep ~extra_dep_suffix:"data" "gcd";
  print_test_ir_and_run_with_dep ~extra_dep_suffix:"data" "array_rev";
  print_test_ir_and_run "float_ops";
  print_test_ir_and_run_with_dep ~extra_dep_suffix:"defn"
    ~extra_dep_with_llvm_backend:true "many_args";
  print_test_ir_and_run "multi_ret";
  print_test_ir_and_run "indirect_call";
  print_test_c ~c_suffix:"defn" "extcalls";
  print_test_run_no_main "data_decl";
  print_test ~extra_subst:[] ~buf ~run:(Some "exn")
    ~tasks:
      ([Ocaml_default "exn_part1"]
      @ ocaml_llvm_and_output_ir "exn_part2"
      @ [Ocaml_default "exn_part3"]);
  print_test_run_no_main "alloc";
  print_test ~extra_subst:[] ~buf ~run:(Some "tailcall")
    ~tasks:([Ocaml_default "tailcall2"] @ ocaml_llvm_and_output_ir "tailcall");
  ()

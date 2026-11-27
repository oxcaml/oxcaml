(* filenames don't contain extensions *)
type task =
  | C of string
  | Ocaml_llvm of string
  | Ocaml_default of string

let rule_of_task = function
  | Ocaml_default filename ->
    Format.sprintf "(run ${ocamlopt} %s.ml -c ${common_flags})" filename
  | Ocaml_llvm filename ->
    Format.sprintf "(run ${ocamlopt} %s.ml -c ${llvm_flags} ${common_flags})"
      filename
  | C filename -> Format.sprintf "(run clang %s.c ${c_flags})" filename

let dependency_of_task = function
  | Ocaml_llvm filename | Ocaml_default filename -> filename ^ ".ml"
  | C filename -> filename ^ ".c"

let target_of_task = function
  | Ocaml_default filename | Ocaml_llvm filename -> filename ^ ".cmx"
  | C filename -> filename ^ ".o"

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
        (fun output -> List.map target_of_task tasks, output ^ ".output")
        run
    in
    let deps =
      List.map dependency_of_task tasks
      @
      if List.exists (function C _ -> true | _ -> false) tasks
      then
        [ "(glob_files %{project_root}/runtime/caml/*.h)";
          "%{project_root}/runtime/caml/domain_state.tbl" ]
      else []
    in
    let targets =
      List.map target_of_task tasks
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
    match run_args with
    | None -> ()
    | Some (_, output) ->
      pp_run_exe_rule ppf ~output;
      pp_compare_output_rule ppf ~output
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
      | "llvm_path" -> "${OXCAML_CLANG}"
      | "llvm_flags" ->
        (* We pass -dno-asm-comments to avoid printing flaky identifiers in Cfg
           instructions *)
        (* CR yusumez: remove -disable-poll-insertion once we can emit poll
           insertions *)
        (* CR yusumez: find a better way to detect LLVM_PATH *)
        "-llvm-backend -llvm-path ${OXCAML_CLANG} -dno-asm-comments \
         -disable-poll-insertion"
      | "common_flags" ->
        "-g -O3 -no-flambda2-simplify-stubs -opaque -S -dump-into-file -dcmm \
         -dcfg -dlinear"
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

let () =
  let buf = Buffer.create 1000 in
  (* [name] compiled with LLVM, and [name_main] natively *)
  let print_test_with_main name =
    print_test ~extra_subst:[] ~buf ~run:(Some name)
      ~tasks:[Ocaml_llvm name; Ocaml_default (name ^ "_main")]
  in
  let print_test_with_dep ~extra_dep_suffix
      ?(extra_dep_with_llvm_backend = false) name =
    let extra_dep_name = name ^ "_" ^ extra_dep_suffix in
    print_test ~extra_subst:[] ~buf ~run:(Some name)
      ~tasks:
        [ (if extra_dep_with_llvm_backend
           then Ocaml_llvm extra_dep_name
           else Ocaml_default extra_dep_name);
          Ocaml_llvm name;
          Ocaml_default (name ^ "_main") ]
  in
  let print_test_c ~c_suffix name =
    print_test ~extra_subst:[] ~buf ~run:(Some name)
      ~tasks:
        [ C (name ^ "_" ^ c_suffix);
          Ocaml_llvm name;
          Ocaml_default (name ^ "_main") ]
  in
  let print_test_no_main name =
    print_test ~extra_subst:[] ~buf ~run:(Some name) ~tasks:[Ocaml_llvm name]
  in
  print_rule ~extra_subst:[] ~buf F.check_env_rule;
  print_test_with_main "const_val";
  print_test ~extra_subst:[] ~buf ~run:(Some "int_ops")
    ~tasks:
      [ C "int_ops_stub";
        Ocaml_default "int_ops_data";
        Ocaml_llvm "int_ops";
        Ocaml_default "int_ops_main" ];
  print_test_with_dep ~extra_dep_suffix:"data" "gcd";
  print_test_with_dep ~extra_dep_suffix:"data" "array_rev";
  print_test_with_main "float_ops";
  print_test_with_dep ~extra_dep_suffix:"defn" ~extra_dep_with_llvm_backend:true
    "many_args";
  print_test_with_main "multi_ret";
  print_test_with_main "indirect_call";
  print_test_c ~c_suffix:"defn" "extcalls";
  print_test_no_main "data_decl";
  print_test ~extra_subst:[] ~buf ~run:(Some "exn")
    ~tasks:
      [ Ocaml_default "exn_part1";
        Ocaml_llvm "exn_part2";
        Ocaml_default "exn_part3" ];
  print_test_no_main "alloc";
  print_test ~extra_subst:[] ~buf ~run:(Some "tailcall")
    ~tasks:[Ocaml_default "tailcall2"; Ocaml_llvm "tailcall"];
  print_test_no_main "switch";
  print_test ~extra_subst:[] ~buf ~run:(Some "csel")
    ~tasks:[C "csel_stub"; Ocaml_llvm "csel"];
  ()

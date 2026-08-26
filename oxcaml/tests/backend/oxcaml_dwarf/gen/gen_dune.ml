let () =
  let enabled_if = {|(enabled_if (= %{context_name} "main"))|} in
  let enabled_if_with_lldb =
    {|(enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))|}
  in
  let enabled_if_without_lldb =
    {|(enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:OXCAML_LLDB=} "")))|}
  in
  let buf = Buffer.create 1000 in
  let subst_common name = function
    | "enabled_if" -> enabled_if
    | "enabled_if_with_lldb" -> enabled_if_with_lldb
    | "enabled_if_without_lldb" -> enabled_if_without_lldb
    | "name" -> name
    | _ -> assert false
  in
  let print_executable name =
    Buffer.add_substitute buf (subst_common name)
      {|
(executable
 (name ${name})
 (modules ${name})
 ${enabled_if}
 (libraries stdlib_stable)
 (ocamlopt_flags (:standard (:include ocamlopt_flags.sexp)))
 (foreign_archives simd_stubs))
|}
  in
  (* Without an LLDB configured, every per-test rule below is disabled, and the
     [runtest-dwarf] alias contains only this single rule, which fails with an
     informative error. *)
  let print_missing_lldb_error () =
    Buffer.clear buf;
    Buffer.add_substitute buf (subst_common "")
      {|
(rule
 (alias runtest-dwarf)
 ${enabled_if_without_lldb}
 (action
  (progn
   (echo
    "ERROR: OXCAML_LLDB environment variable not set.\n\
DWARF tests require a custom LLDB build. Please set OXCAML_LLDB to \
the path of your custom LLDB binary.\n\
Example: export OXCAML_LLDB=/path/to/custom/lldb")
   (bash "exit 1"))))
|};
    Buffer.output_buffer Out_channel.stdout buf
  in
  (* Function to generate rules for executable tests that produce output *)
  let print_dwarf_test ?(extra_deps = []) name =
    (* Leading "" yields a space after [${filter}], or "" when empty. *)
    let extra_deps = String.concat " " ("" :: extra_deps) in
    let subst = function
      | "filter" -> "filter_for_function_call_only.sh"
      | "extra_deps" -> extra_deps
      | key -> subst_common name key
    in
    Buffer.clear buf;
    print_executable name;
    Buffer.add_substitute buf subst
      {|
(rule
 ${enabled_if_with_lldb}
 (targets ${name}.output.corrected)
 (deps ${name}.exe ${name}.lldb ${filter}${extra_deps})
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' ${name}.lldb > \
     ${name}_clean.lldb")
   (with-outputs-to ${name}.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s ${name}_clean.lldb ./${name}.exe)
     (run sh ./${filter}))))))

(rule
 (alias runtest-dwarf)
 ${enabled_if_with_lldb}
 (deps ${name}.output ${name}.output.corrected)
 (action (diff ${name}.output ${name}.output.corrected)))
|};
    Buffer.output_buffer Out_channel.stdout buf
  in
  (* Function to generate rules for tests driven by a Python script running
     inside LLDB's embedded interpreter (see lldb_test_utils.py). Pass/fail is
     the exit code: an exception escaping the script fails the [command script
     import], which in batch mode makes LLDB exit nonzero. *)
  let print_dwarf_python_test name =
    Buffer.clear buf;
    print_executable name;
    Buffer.add_substitute buf (subst_common name)
      {|
(rule
 (alias runtest-dwarf)
 ${enabled_if_with_lldb}
 (deps ${name}.exe ${name}.py ${name}.ml lldb_test_utils.py)
 (action
  (run %{env:OXCAML_LLDB=} --batch -o "command script import ${name}.py")))
|};
    Buffer.output_buffer Out_channel.stdout buf
  in
  (* Like [print_dwarf_python_test], but for a test whose executable inlines
     code from compilation units built in a *different working directory*, which
     a dune [executable] stanza cannot express: dune compiles every module of a
     stanza from the workspace root, so all units agree on how relative paths
     resolve, masking bugs in cross-directory file-path handling (and its dev
     profile appends [-opaque], which would defeat cross-unit inlining anyway).
     The custom rule below instead compiles the modules of [lib_modules] from
     within [lib_dir] -- each unit with a [-directory] argument identifying its
     source directory, as directory-at-a-time build systems do -- and then
     compiles and links [name] manually.  Each [lib_modules] entry pairs a
     source basename with the compilation-unit name to pass to [-o]. *)
  let print_cross_unit_dwarf_python_test name ~lib_dir ~lib_modules =
    (* [(:include ...)] is only available in field position, so the flags in
       ocamlopt_flags.sexp cannot be spliced directly into the rule's action.
       Instead, the action's first step extracts the flags from that file at
       build time (stripping comment lines and the surrounding parentheses) into
       flags.txt, and each compilation step is a [system] action that splices
       them back in with [$(cat flags.txt)], keeping them in sync with the
       executable stanzas automatically. [path] locates flags.txt relative to
       the directory the compilation step runs in. (The [$(...)] must only ever
       appear in substitution *values*: a literal [$(...)] in the template
       itself would be parsed by [Buffer.add_substitute].) *)
    let flags_from path = Printf.sprintf "$(cat %s)" path in
    let lib_mls =
      String.concat " "
        (List.map
           (fun (source, _unit) -> Printf.sprintf "%s/%s.ml" lib_dir source)
           lib_modules)
    in
    let lib_cmxs =
      String.concat " "
        (List.map
           (fun (_source, unit_name) ->
             Printf.sprintf "%s/%s.cmx" lib_dir unit_name)
           lib_modules)
    in
    let lib_compile_runs =
      String.concat "\n"
        (List.map
           (fun (source, unit_name) ->
             Printf.sprintf
               "      (system \"%%{bin:ocamlopt.opt} %s -directory \
                oxcaml_dwarf/%s -I . -o %s.cmx -c %s.ml\")"
               (flags_from "../flags.txt")
               lib_dir unit_name source)
           lib_modules)
    in
    let subst = function
      | "flags" -> flags_from "flags.txt"
      | "lib_dir" -> lib_dir
      | "lib_mls" -> lib_mls
      | "lib_cmxs" -> lib_cmxs
      | "lib_compile_runs" -> lib_compile_runs
      | key -> subst_common name key
    in
    Buffer.clear buf;
    Buffer.add_substitute buf subst
      {|
(rule
 ${enabled_if}
 (targets ${name}.exe)
 (deps ${lib_mls} ${name}.mli ${name}.ml ${name}_main.ml ocamlopt_flags.sexp)
 (action
  (no-infer
   (progn
    (system "grep -v '^;' ocamlopt_flags.sexp | tr -d '()' > flags.txt")
    (chdir ${lib_dir}
     (progn
${lib_compile_runs}))
    (system "%{bin:ocamlopt.opt} ${flags} -directory oxcaml_dwarf -I ${lib_dir} -c ${name}.mli")
    (system "%{bin:ocamlopt.opt} ${flags} -directory oxcaml_dwarf -I ${lib_dir} -c ${name}.ml")
    (system "%{bin:ocamlopt.opt} ${flags} -directory oxcaml_dwarf -I ${lib_dir} -c ${name}_main.ml")
    (system "%{bin:ocamlopt.opt} ${flags} ${lib_cmxs} ${name}.cmx ${name}_main.cmx -o ${name}.exe")))))

(rule
 (alias runtest-dwarf)
 ${enabled_if_with_lldb}
 (deps ${name}.exe ${name}.py ${name}.mli ${name}.ml ${name}_main.ml ${lib_mls} lldb_test_utils.py)
 (action
  (run %{env:OXCAML_LLDB=} --batch -o "command script import ${name}.py")))
|};
    Buffer.output_buffer Out_channel.stdout buf
  in
  print_missing_lldb_error ();
  (* Generate tests - add more tests here as needed *)
  print_dwarf_test "test_basic_dwarf";
  print_dwarf_test "test_unboxed_dwarf";
  print_dwarf_test "test_datatypes_dwarf";
  print_dwarf_test "test_simd_dwarf";
  print_dwarf_test "test_simple_functor_dwarf";
  print_dwarf_test "test_parameters_dwarf";
  print_dwarf_test "test_callstack_dwarf";
  print_dwarf_test "test_stepping_dwarf";
  print_dwarf_test "test_closures_dwarf";
  print_dwarf_test "test_large_data_dwarf";
  print_dwarf_test "test_tailrec_dwarf";
  print_dwarf_test "test_ocaml_and_c_dwarf" ~extra_deps:["frames.py"];
  print_dwarf_python_test "test_inlined_frames_dwarf";
  print_cross_unit_dwarf_python_test "test_cross_unit_paths_dwarf"
    ~lib_dir:"cross_unit_dir"
    ~lib_modules:
      [ "cu_lib_inner", "cu_lib_inner";
        "cu_lib_outer", "cu_lib_outer";
        "test_cross_unit_paths_dwarf", "cu_prim"
      ];
  ()

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
 ${enabled_if_without_lldb}
 (targets ${name}.output.corrected)
 (deps ${name}.exe)
 (action
  (progn
   (echo
    "ERROR: OXCAML_LLDB environment variable not set.\n\
DWARF tests require a custom LLDB build. Please set OXCAML_LLDB to \
the path of your custom LLDB binary.\n\
Example: export OXCAML_LLDB=/path/to/custom/lldb")
   (bash "exit 1"))))

(rule
 (alias runtest-dwarf)
 ${enabled_if}
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
  ()

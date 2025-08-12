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
  (* Function to generate rules for executable tests that produce output *)
  let print_dwarf_test name =
    let subst = function
      | "enabled_if" -> enabled_if
      | "enabled_if_with_lldb" -> enabled_if_with_lldb
      | "enabled_if_without_lldb" -> enabled_if_without_lldb
      | "name" -> name
      | "filter" -> "filter.sh"
      | _ -> assert false
    in
    Buffer.clear buf;
    Buffer.add_substitute buf subst
      {|
(executable
 (name ${name})
 (modules ${name})
 ${enabled_if}
 (ocamlopt_flags
  (:standard -g -gno-upstream-dwarf -bin-annot-cms -extension simd_beta))
 (foreign_archives simd_stubs))

(rule
 ${enabled_if_with_lldb}
 (targets ${name}.output.corrected)
 (deps ${name}.exe ${name}.lldb ${filter})
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
  (* Generate tests - add more tests here as needed *)
  print_dwarf_test "test_basic_dwarf";
  print_dwarf_test "test_unboxed_dwarf";
  print_dwarf_test "test_datatypes_dwarf";
  print_dwarf_test "test_simd_dwarf"

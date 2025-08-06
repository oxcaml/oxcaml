let () =
  let enabled_if =
    {|(enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:TEST_OXCAML_DWARF=false} "true")))|}
  in
  let enabled_if_with_lldb =
    {|(enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:TEST_OXCAML_DWARF=false} "true")
   (<> %{env:OXCAML_LLDB=} "")))|}
  in
  let buf = Buffer.create 1000 in
  
  (* Function to generate rules for executable tests that produce output *)
  let print_dwarf_test name =
    let subst = function
      | "enabled_if" -> enabled_if
      | "enabled_if_with_lldb" -> enabled_if_with_lldb
      | "name" -> name
      | "filter" -> "filter.sh"
      | _ -> assert false
    in
    Buffer.clear buf;
    Buffer.add_substitute buf subst
      {|
(rule
 ${enabled_if}
 (targets ${name}.exe)
 (deps ${name}.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -gno-upstream-dwarf -bin-annot-cms -o ${name}.exe)))

(rule
 ${enabled_if_with_lldb}
 (targets ${name}.output.corrected)
 (deps ${name}.exe ${name}.lldb ${filter})
 (action
  (progn
   (bash "sed 's/^(lldb) //' ${name}.lldb > ${name}_clean.lldb")
   (with-outputs-to ${name}.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=lldb} -s ${name}_clean.lldb ./${name}.exe)
     (run sh ./${filter}))))))

(rule
 (alias runtest)
 ${enabled_if}
 (deps ${name}.output ${name}.output.corrected)
 (action (diff ${name}.output ${name}.output.corrected)))
|};
    Buffer.output_buffer Out_channel.stdout buf
  in
  
  
  (* Generate tests - add more tests here as needed *)
  print_dwarf_test "test_basic_dwarf";
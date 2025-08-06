
(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:TEST_OXCAML_DWARF=false} "true")))
 (targets test_basic_dwarf.exe)
 (deps test_basic_dwarf.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -gno-upstream-dwarf -bin-annot-cms -o test_basic_dwarf.exe)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:TEST_OXCAML_DWARF=false} "true")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_basic_dwarf.output.corrected)
 (deps test_basic_dwarf.exe test_basic_dwarf.lldb filter.sh)
 (action
  (progn
   (bash "sed 's/^(lldb) //' test_basic_dwarf.lldb > test_basic_dwarf_clean.lldb")
   (with-outputs-to test_basic_dwarf.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=lldb} -s test_basic_dwarf_clean.lldb ./test_basic_dwarf.exe)
     (run sh ./filter.sh))))))

(rule
 (alias runtest)
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:TEST_OXCAML_DWARF=false} "true")))
 (deps test_basic_dwarf.output test_basic_dwarf.output.corrected)
 (action (diff test_basic_dwarf.output test_basic_dwarf.output.corrected)))

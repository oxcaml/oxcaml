
(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_basic_dwarf.exe)
 (deps test_basic_dwarf.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -gno-upstream-dwarf -bin-annot-cms -o test_basic_dwarf.exe)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_basic_dwarf.output.corrected)
 (deps test_basic_dwarf.exe test_basic_dwarf.lldb filter.sh)
 (action
  (progn
   (bash "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_basic_dwarf.lldb > test_basic_dwarf_clean.lldb")
   (with-outputs-to test_basic_dwarf.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s test_basic_dwarf_clean.lldb ./test_basic_dwarf.exe)
     (run sh ./filter.sh))))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:OXCAML_LLDB=} "")))
 (targets test_basic_dwarf.output.corrected)
 (deps test_basic_dwarf.exe)
 (action
  (progn
   (echo "ERROR: OXCAML_LLDB environment variable not set.\nDWARF tests require a custom LLDB build. Please set OXCAML_LLDB to the path of your custom LLDB binary.\nExample: export OXCAML_LLDB=/path/to/custom/lldb")
   (bash "exit 1"))))

(rule
 (alias runtest-dwarf)
 (enabled_if (= %{context_name} "main"))
 (deps test_basic_dwarf.output test_basic_dwarf.output.corrected)
 (action (diff test_basic_dwarf.output test_basic_dwarf.output.corrected)))

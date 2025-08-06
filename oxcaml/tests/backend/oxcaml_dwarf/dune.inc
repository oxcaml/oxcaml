
(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:TEST_OXCAML_DWARF=false} "true")))
 (targets test_basic_dwarf.exe)
 (deps test_basic_dwarf.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -o test_basic_dwarf.exe)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:TEST_OXCAML_DWARF=false} "true")))
 (targets test_basic_dwarf.output.corrected)
 (deps test_basic_dwarf.exe)
 (action
  (with-outputs-to test_basic_dwarf.output.corrected
   (run ./test_basic_dwarf.exe))))

(rule
 (alias runtest)
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:TEST_OXCAML_DWARF=false} "true")))
 (deps test_basic_dwarf.output test_basic_dwarf.output.corrected)
 (action (diff test_basic_dwarf.output test_basic_dwarf.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:TEST_OXCAML_DWARF=false} "true")))
 (targets test_dwarf_functions.exe)
 (deps test_dwarf_functions.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -o test_dwarf_functions.exe)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:TEST_OXCAML_DWARF=false} "true")))
 (targets test_dwarf_functions.output.corrected)
 (deps test_dwarf_functions.exe)
 (action
  (with-outputs-to test_dwarf_functions.output.corrected
   (run ./test_dwarf_functions.exe))))

(rule
 (alias runtest)
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:TEST_OXCAML_DWARF=false} "true")))
 (deps test_dwarf_functions.output test_dwarf_functions.output.corrected)
 (action (diff test_dwarf_functions.output test_dwarf_functions.output.corrected)))

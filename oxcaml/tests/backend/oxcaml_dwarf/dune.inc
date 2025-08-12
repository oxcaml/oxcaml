
(executable
 (name test_basic_dwarf)
 (modules test_basic_dwarf)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -g -gno-upstream-dwarf -bin-annot-cms -extension simd_beta))
 (foreign_archives simd_stubs))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_basic_dwarf.output.corrected)
 (deps test_basic_dwarf.exe test_basic_dwarf.lldb filter.sh)
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_basic_dwarf.lldb > \
     test_basic_dwarf_clean.lldb")
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
   (echo
    "ERROR: OXCAML_LLDB environment variable not set.\n\
DWARF tests require a custom LLDB build. Please set OXCAML_LLDB to \
the path of your custom LLDB binary.\n\
Example: export OXCAML_LLDB=/path/to/custom/lldb")
   (bash "exit 1"))))

(rule
 (alias runtest-dwarf)
 (enabled_if (= %{context_name} "main"))
 (deps test_basic_dwarf.output test_basic_dwarf.output.corrected)
 (action (diff test_basic_dwarf.output test_basic_dwarf.output.corrected)))

(executable
 (name test_unboxed_dwarf)
 (modules test_unboxed_dwarf)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -g -gno-upstream-dwarf -bin-annot-cms -extension simd_beta))
 (foreign_archives simd_stubs))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_unboxed_dwarf.output.corrected)
 (deps test_unboxed_dwarf.exe test_unboxed_dwarf.lldb filter.sh)
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_unboxed_dwarf.lldb > \
     test_unboxed_dwarf_clean.lldb")
   (with-outputs-to test_unboxed_dwarf.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s test_unboxed_dwarf_clean.lldb ./test_unboxed_dwarf.exe)
     (run sh ./filter.sh))))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:OXCAML_LLDB=} "")))
 (targets test_unboxed_dwarf.output.corrected)
 (deps test_unboxed_dwarf.exe)
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
 (enabled_if (= %{context_name} "main"))
 (deps test_unboxed_dwarf.output test_unboxed_dwarf.output.corrected)
 (action (diff test_unboxed_dwarf.output test_unboxed_dwarf.output.corrected)))

(executable
 (name test_datatypes_dwarf)
 (modules test_datatypes_dwarf)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -g -gno-upstream-dwarf -bin-annot-cms -extension simd_beta))
 (foreign_archives simd_stubs))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_datatypes_dwarf.output.corrected)
 (deps test_datatypes_dwarf.exe test_datatypes_dwarf.lldb filter.sh)
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_datatypes_dwarf.lldb > \
     test_datatypes_dwarf_clean.lldb")
   (with-outputs-to test_datatypes_dwarf.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s test_datatypes_dwarf_clean.lldb ./test_datatypes_dwarf.exe)
     (run sh ./filter.sh))))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:OXCAML_LLDB=} "")))
 (targets test_datatypes_dwarf.output.corrected)
 (deps test_datatypes_dwarf.exe)
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
 (enabled_if (= %{context_name} "main"))
 (deps test_datatypes_dwarf.output test_datatypes_dwarf.output.corrected)
 (action (diff test_datatypes_dwarf.output test_datatypes_dwarf.output.corrected)))

(executable
 (name test_simd_dwarf)
 (modules test_simd_dwarf)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -g -gno-upstream-dwarf -bin-annot-cms -extension simd_beta))
 (foreign_archives simd_stubs))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_simd_dwarf.output.corrected)
 (deps test_simd_dwarf.exe test_simd_dwarf.lldb filter.sh)
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_simd_dwarf.lldb > \
     test_simd_dwarf_clean.lldb")
   (with-outputs-to test_simd_dwarf.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s test_simd_dwarf_clean.lldb ./test_simd_dwarf.exe)
     (run sh ./filter.sh))))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:OXCAML_LLDB=} "")))
 (targets test_simd_dwarf.output.corrected)
 (deps test_simd_dwarf.exe)
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
 (enabled_if (= %{context_name} "main"))
 (deps test_simd_dwarf.output test_simd_dwarf.output.corrected)
 (action (diff test_simd_dwarf.output test_simd_dwarf.output.corrected)))

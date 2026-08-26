
(rule
 (alias runtest-dwarf)
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{env:OXCAML_LLDB=} "")))
 (action
  (progn
   (echo
    "ERROR: OXCAML_LLDB environment variable not set.\n\
DWARF tests require a custom LLDB build. Please set OXCAML_LLDB to \
the path of your custom LLDB binary.\n\
Example: export OXCAML_LLDB=/path/to/custom/lldb")
   (bash "exit 1"))))

(executable
 (name test_basic_dwarf)
 (modules test_basic_dwarf)
 (enabled_if (= %{context_name} "main"))
 (libraries stdlib_stable)
 (ocamlopt_flags (:standard (:include ocamlopt_flags.sexp)))
 (foreign_archives simd_stubs))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_basic_dwarf.output.corrected)
 (deps test_basic_dwarf.exe test_basic_dwarf.lldb filter_for_function_call_only.sh)
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_basic_dwarf.lldb > \
     test_basic_dwarf_clean.lldb")
   (with-outputs-to test_basic_dwarf.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s test_basic_dwarf_clean.lldb ./test_basic_dwarf.exe)
     (run sh ./filter_for_function_call_only.sh))))))

(rule
 (alias runtest-dwarf)
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (deps test_basic_dwarf.output test_basic_dwarf.output.corrected)
 (action (diff test_basic_dwarf.output test_basic_dwarf.output.corrected)))

(executable
 (name test_unboxed_dwarf)
 (modules test_unboxed_dwarf)
 (enabled_if (= %{context_name} "main"))
 (libraries stdlib_stable)
 (ocamlopt_flags (:standard (:include ocamlopt_flags.sexp)))
 (foreign_archives simd_stubs))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_unboxed_dwarf.output.corrected)
 (deps test_unboxed_dwarf.exe test_unboxed_dwarf.lldb filter_for_function_call_only.sh)
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_unboxed_dwarf.lldb > \
     test_unboxed_dwarf_clean.lldb")
   (with-outputs-to test_unboxed_dwarf.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s test_unboxed_dwarf_clean.lldb ./test_unboxed_dwarf.exe)
     (run sh ./filter_for_function_call_only.sh))))))

(rule
 (alias runtest-dwarf)
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (deps test_unboxed_dwarf.output test_unboxed_dwarf.output.corrected)
 (action (diff test_unboxed_dwarf.output test_unboxed_dwarf.output.corrected)))

(executable
 (name test_datatypes_dwarf)
 (modules test_datatypes_dwarf)
 (enabled_if (= %{context_name} "main"))
 (libraries stdlib_stable)
 (ocamlopt_flags (:standard (:include ocamlopt_flags.sexp)))
 (foreign_archives simd_stubs))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_datatypes_dwarf.output.corrected)
 (deps test_datatypes_dwarf.exe test_datatypes_dwarf.lldb filter_for_function_call_only.sh)
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_datatypes_dwarf.lldb > \
     test_datatypes_dwarf_clean.lldb")
   (with-outputs-to test_datatypes_dwarf.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s test_datatypes_dwarf_clean.lldb ./test_datatypes_dwarf.exe)
     (run sh ./filter_for_function_call_only.sh))))))

(rule
 (alias runtest-dwarf)
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (deps test_datatypes_dwarf.output test_datatypes_dwarf.output.corrected)
 (action (diff test_datatypes_dwarf.output test_datatypes_dwarf.output.corrected)))

(executable
 (name test_simd_dwarf)
 (modules test_simd_dwarf)
 (enabled_if (= %{context_name} "main"))
 (libraries stdlib_stable)
 (ocamlopt_flags (:standard (:include ocamlopt_flags.sexp)))
 (foreign_archives simd_stubs))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_simd_dwarf.output.corrected)
 (deps test_simd_dwarf.exe test_simd_dwarf.lldb filter_for_function_call_only.sh)
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_simd_dwarf.lldb > \
     test_simd_dwarf_clean.lldb")
   (with-outputs-to test_simd_dwarf.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s test_simd_dwarf_clean.lldb ./test_simd_dwarf.exe)
     (run sh ./filter_for_function_call_only.sh))))))

(rule
 (alias runtest-dwarf)
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (deps test_simd_dwarf.output test_simd_dwarf.output.corrected)
 (action (diff test_simd_dwarf.output test_simd_dwarf.output.corrected)))

(executable
 (name test_simple_functor_dwarf)
 (modules test_simple_functor_dwarf)
 (enabled_if (= %{context_name} "main"))
 (libraries stdlib_stable)
 (ocamlopt_flags (:standard (:include ocamlopt_flags.sexp)))
 (foreign_archives simd_stubs))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_simple_functor_dwarf.output.corrected)
 (deps test_simple_functor_dwarf.exe test_simple_functor_dwarf.lldb filter_for_function_call_only.sh)
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_simple_functor_dwarf.lldb > \
     test_simple_functor_dwarf_clean.lldb")
   (with-outputs-to test_simple_functor_dwarf.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s test_simple_functor_dwarf_clean.lldb ./test_simple_functor_dwarf.exe)
     (run sh ./filter_for_function_call_only.sh))))))

(rule
 (alias runtest-dwarf)
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (deps test_simple_functor_dwarf.output test_simple_functor_dwarf.output.corrected)
 (action (diff test_simple_functor_dwarf.output test_simple_functor_dwarf.output.corrected)))

(executable
 (name test_parameters_dwarf)
 (modules test_parameters_dwarf)
 (enabled_if (= %{context_name} "main"))
 (libraries stdlib_stable)
 (ocamlopt_flags (:standard (:include ocamlopt_flags.sexp)))
 (foreign_archives simd_stubs))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_parameters_dwarf.output.corrected)
 (deps test_parameters_dwarf.exe test_parameters_dwarf.lldb filter_for_function_call_only.sh)
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_parameters_dwarf.lldb > \
     test_parameters_dwarf_clean.lldb")
   (with-outputs-to test_parameters_dwarf.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s test_parameters_dwarf_clean.lldb ./test_parameters_dwarf.exe)
     (run sh ./filter_for_function_call_only.sh))))))

(rule
 (alias runtest-dwarf)
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (deps test_parameters_dwarf.output test_parameters_dwarf.output.corrected)
 (action (diff test_parameters_dwarf.output test_parameters_dwarf.output.corrected)))

(executable
 (name test_callstack_dwarf)
 (modules test_callstack_dwarf)
 (enabled_if (= %{context_name} "main"))
 (libraries stdlib_stable)
 (ocamlopt_flags (:standard (:include ocamlopt_flags.sexp)))
 (foreign_archives simd_stubs))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_callstack_dwarf.output.corrected)
 (deps test_callstack_dwarf.exe test_callstack_dwarf.lldb filter_for_function_call_only.sh)
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_callstack_dwarf.lldb > \
     test_callstack_dwarf_clean.lldb")
   (with-outputs-to test_callstack_dwarf.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s test_callstack_dwarf_clean.lldb ./test_callstack_dwarf.exe)
     (run sh ./filter_for_function_call_only.sh))))))

(rule
 (alias runtest-dwarf)
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (deps test_callstack_dwarf.output test_callstack_dwarf.output.corrected)
 (action (diff test_callstack_dwarf.output test_callstack_dwarf.output.corrected)))

(executable
 (name test_stepping_dwarf)
 (modules test_stepping_dwarf)
 (enabled_if (= %{context_name} "main"))
 (libraries stdlib_stable)
 (ocamlopt_flags (:standard (:include ocamlopt_flags.sexp)))
 (foreign_archives simd_stubs))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_stepping_dwarf.output.corrected)
 (deps test_stepping_dwarf.exe test_stepping_dwarf.lldb filter_for_function_call_only.sh)
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_stepping_dwarf.lldb > \
     test_stepping_dwarf_clean.lldb")
   (with-outputs-to test_stepping_dwarf.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s test_stepping_dwarf_clean.lldb ./test_stepping_dwarf.exe)
     (run sh ./filter_for_function_call_only.sh))))))

(rule
 (alias runtest-dwarf)
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (deps test_stepping_dwarf.output test_stepping_dwarf.output.corrected)
 (action (diff test_stepping_dwarf.output test_stepping_dwarf.output.corrected)))

(executable
 (name test_closures_dwarf)
 (modules test_closures_dwarf)
 (enabled_if (= %{context_name} "main"))
 (libraries stdlib_stable)
 (ocamlopt_flags (:standard (:include ocamlopt_flags.sexp)))
 (foreign_archives simd_stubs))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_closures_dwarf.output.corrected)
 (deps test_closures_dwarf.exe test_closures_dwarf.lldb filter_for_function_call_only.sh)
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_closures_dwarf.lldb > \
     test_closures_dwarf_clean.lldb")
   (with-outputs-to test_closures_dwarf.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s test_closures_dwarf_clean.lldb ./test_closures_dwarf.exe)
     (run sh ./filter_for_function_call_only.sh))))))

(rule
 (alias runtest-dwarf)
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (deps test_closures_dwarf.output test_closures_dwarf.output.corrected)
 (action (diff test_closures_dwarf.output test_closures_dwarf.output.corrected)))

(executable
 (name test_large_data_dwarf)
 (modules test_large_data_dwarf)
 (enabled_if (= %{context_name} "main"))
 (libraries stdlib_stable)
 (ocamlopt_flags (:standard (:include ocamlopt_flags.sexp)))
 (foreign_archives simd_stubs))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_large_data_dwarf.output.corrected)
 (deps test_large_data_dwarf.exe test_large_data_dwarf.lldb filter_for_function_call_only.sh)
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_large_data_dwarf.lldb > \
     test_large_data_dwarf_clean.lldb")
   (with-outputs-to test_large_data_dwarf.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s test_large_data_dwarf_clean.lldb ./test_large_data_dwarf.exe)
     (run sh ./filter_for_function_call_only.sh))))))

(rule
 (alias runtest-dwarf)
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (deps test_large_data_dwarf.output test_large_data_dwarf.output.corrected)
 (action (diff test_large_data_dwarf.output test_large_data_dwarf.output.corrected)))

(executable
 (name test_tailrec_dwarf)
 (modules test_tailrec_dwarf)
 (enabled_if (= %{context_name} "main"))
 (libraries stdlib_stable)
 (ocamlopt_flags (:standard (:include ocamlopt_flags.sexp)))
 (foreign_archives simd_stubs))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_tailrec_dwarf.output.corrected)
 (deps test_tailrec_dwarf.exe test_tailrec_dwarf.lldb filter_for_function_call_only.sh)
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_tailrec_dwarf.lldb > \
     test_tailrec_dwarf_clean.lldb")
   (with-outputs-to test_tailrec_dwarf.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s test_tailrec_dwarf_clean.lldb ./test_tailrec_dwarf.exe)
     (run sh ./filter_for_function_call_only.sh))))))

(rule
 (alias runtest-dwarf)
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (deps test_tailrec_dwarf.output test_tailrec_dwarf.output.corrected)
 (action (diff test_tailrec_dwarf.output test_tailrec_dwarf.output.corrected)))

(executable
 (name test_ocaml_and_c_dwarf)
 (modules test_ocaml_and_c_dwarf)
 (enabled_if (= %{context_name} "main"))
 (libraries stdlib_stable)
 (ocamlopt_flags (:standard (:include ocamlopt_flags.sexp)))
 (foreign_archives simd_stubs))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (targets test_ocaml_and_c_dwarf.output.corrected)
 (deps test_ocaml_and_c_dwarf.exe test_ocaml_and_c_dwarf.lldb filter_for_function_call_only.sh frames.py)
 (action
  (progn
   (bash
    "sed -e 's/^(lldb) //' -e '/^[[:space:]]*$/d' test_ocaml_and_c_dwarf.lldb > \
     test_ocaml_and_c_dwarf_clean.lldb")
   (with-outputs-to test_ocaml_and_c_dwarf.output.corrected
    (pipe-outputs
     (run %{env:OXCAML_LLDB=} -s test_ocaml_and_c_dwarf_clean.lldb ./test_ocaml_and_c_dwarf.exe)
     (run sh ./filter_for_function_call_only.sh))))))

(rule
 (alias runtest-dwarf)
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (deps test_ocaml_and_c_dwarf.output test_ocaml_and_c_dwarf.output.corrected)
 (action (diff test_ocaml_and_c_dwarf.output test_ocaml_and_c_dwarf.output.corrected)))

(executable
 (name test_inlined_frames_dwarf)
 (modules test_inlined_frames_dwarf)
 (enabled_if (= %{context_name} "main"))
 (libraries stdlib_stable)
 (ocamlopt_flags (:standard (:include ocamlopt_flags.sexp)))
 (foreign_archives simd_stubs))

(rule
 (alias runtest-dwarf)
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (deps test_inlined_frames_dwarf.exe test_inlined_frames_dwarf.py test_inlined_frames_dwarf.ml lldb_test_utils.py)
 (action
  (run %{env:OXCAML_LLDB=} --batch -o "command script import test_inlined_frames_dwarf.py")))

(rule
 (enabled_if (= %{context_name} "main"))
 (targets test_cross_unit_paths_dwarf.exe)
 (deps cross_unit_dir/cu_lib_inner.ml cross_unit_dir/cu_lib_outer.ml cross_unit_dir/test_cross_unit_paths_dwarf.ml test_cross_unit_paths_dwarf.mli test_cross_unit_paths_dwarf.ml test_cross_unit_paths_dwarf_main.ml ocamlopt_flags.sexp)
 (action
  (no-infer
   (progn
    (system "grep -v '^;' ocamlopt_flags.sexp | tr -d '()' > flags.txt")
    (chdir cross_unit_dir
     (progn
      (system "%{bin:ocamlopt.opt} $(cat ../flags.txt) -directory oxcaml_dwarf/cross_unit_dir -I . -o cu_lib_inner.cmx -c cu_lib_inner.ml")
      (system "%{bin:ocamlopt.opt} $(cat ../flags.txt) -directory oxcaml_dwarf/cross_unit_dir -I . -o cu_lib_outer.cmx -c cu_lib_outer.ml")
      (system "%{bin:ocamlopt.opt} $(cat ../flags.txt) -directory oxcaml_dwarf/cross_unit_dir -I . -o cu_prim.cmx -c test_cross_unit_paths_dwarf.ml")))
    (system "%{bin:ocamlopt.opt} $(cat flags.txt) -directory oxcaml_dwarf -I cross_unit_dir -c test_cross_unit_paths_dwarf.mli")
    (system "%{bin:ocamlopt.opt} $(cat flags.txt) -directory oxcaml_dwarf -I cross_unit_dir -c test_cross_unit_paths_dwarf.ml")
    (system "%{bin:ocamlopt.opt} $(cat flags.txt) -directory oxcaml_dwarf -I cross_unit_dir -c test_cross_unit_paths_dwarf_main.ml")
    (system "%{bin:ocamlopt.opt} $(cat flags.txt) cross_unit_dir/cu_lib_inner.cmx cross_unit_dir/cu_lib_outer.cmx cross_unit_dir/cu_prim.cmx test_cross_unit_paths_dwarf.cmx test_cross_unit_paths_dwarf_main.cmx -o test_cross_unit_paths_dwarf.exe")))))

(rule
 (alias runtest-dwarf)
 (enabled_if
  (and
   (= %{context_name} "main")
   (<> %{env:OXCAML_LLDB=} "")))
 (deps test_cross_unit_paths_dwarf.exe test_cross_unit_paths_dwarf.py test_cross_unit_paths_dwarf.mli test_cross_unit_paths_dwarf.ml test_cross_unit_paths_dwarf_main.ml cross_unit_dir/cu_lib_inner.ml cross_unit_dir/cu_lib_outer.ml cross_unit_dir/test_cross_unit_paths_dwarf.ml lldb_test_utils.py)
 (action
  (run %{env:OXCAML_LLDB=} --batch -o "command script import test_cross_unit_paths_dwarf.py")))

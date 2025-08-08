(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (= %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (action
  (progn
   (echo "ERROR: OXCAML_CLANG environment variable not set.\nLlvmize tests require a custom LLVM build.\nPlease set OXCAML_CLANG to the path of your custom Clang binary.\nExample: export OXCAML_CLANG=/path/to/custom/clang\n")
   (bash "exit 1"))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (targets id_fn.output.corrected)
 (deps id_fn.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} id_fn.ml -c -llvm-backend -llvm-path ${OXCAML_CLANG} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -dump-into-file -dcmm -dcfg -stop-after llvmize)
   (with-outputs-to id_fn.output.corrected (pipe-outputs (run cat id_fn.ll) (run ./filter.sh))))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps id_fn.output id_fn.output.corrected)
 (action
  (diff id_fn.output id_fn.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (targets const_val.cmx const_val_ir.output.corrected const_val_main.cmx const_val.output.exe)
 (deps const_val.ml const_val_main.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} const_val.ml -c -llvm-backend -llvm-path ${OXCAML_CLANG} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to const_val_ir.output.corrected (pipe-outputs (run cat const_val.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} const_val_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} const_val.cmx const_val_main.cmx -opaque -o const_val.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps const_val.cmx const_val_main.cmx)
 (targets const_val.output.corrected)
 (action
  (with-outputs-to
   const_val.output.corrected
   (run ./const_val.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps const_val.output const_val.output.corrected)
 (action
  (diff const_val.output const_val.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps const_val_ir.output const_val_ir.output.corrected)
 (action
  (diff const_val_ir.output const_val_ir.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (targets int_ops_data.cmx int_ops.cmx int_ops_ir.output.corrected int_ops_main.cmx int_ops.output.exe)
 (deps int_ops_data.ml int_ops.ml int_ops_main.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} int_ops_data.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} int_ops.ml -c -llvm-backend -llvm-path ${OXCAML_CLANG} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to int_ops_ir.output.corrected (pipe-outputs (run cat int_ops.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} int_ops_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} int_ops_data.cmx int_ops.cmx int_ops_main.cmx -opaque -o int_ops.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps int_ops_data.cmx int_ops.cmx int_ops_main.cmx)
 (targets int_ops.output.corrected)
 (action
  (with-outputs-to
   int_ops.output.corrected
   (run ./int_ops.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps int_ops.output int_ops.output.corrected)
 (action
  (diff int_ops.output int_ops.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps int_ops_ir.output int_ops_ir.output.corrected)
 (action
  (diff int_ops_ir.output int_ops_ir.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (targets gcd_data.cmx gcd.cmx gcd_ir.output.corrected gcd_main.cmx gcd.output.exe)
 (deps gcd_data.ml gcd.ml gcd_main.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} gcd_data.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} gcd.ml -c -llvm-backend -llvm-path ${OXCAML_CLANG} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to gcd_ir.output.corrected (pipe-outputs (run cat gcd.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} gcd_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} gcd_data.cmx gcd.cmx gcd_main.cmx -opaque -o gcd.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps gcd_data.cmx gcd.cmx gcd_main.cmx)
 (targets gcd.output.corrected)
 (action
  (with-outputs-to
   gcd.output.corrected
   (run ./gcd.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps gcd.output gcd.output.corrected)
 (action
  (diff gcd.output gcd.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps gcd_ir.output gcd_ir.output.corrected)
 (action
  (diff gcd_ir.output gcd_ir.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (targets array_rev_data.cmx array_rev.cmx array_rev_ir.output.corrected array_rev_main.cmx array_rev.output.exe)
 (deps array_rev_data.ml array_rev.ml array_rev_main.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} array_rev_data.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} array_rev.ml -c -llvm-backend -llvm-path ${OXCAML_CLANG} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to array_rev_ir.output.corrected (pipe-outputs (run cat array_rev.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} array_rev_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} array_rev_data.cmx array_rev.cmx array_rev_main.cmx -opaque -o array_rev.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps array_rev_data.cmx array_rev.cmx array_rev_main.cmx)
 (targets array_rev.output.corrected)
 (action
  (with-outputs-to
   array_rev.output.corrected
   (run ./array_rev.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps array_rev.output array_rev.output.corrected)
 (action
  (diff array_rev.output array_rev.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps array_rev_ir.output array_rev_ir.output.corrected)
 (action
  (diff array_rev_ir.output array_rev_ir.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (targets float_ops.cmx float_ops_ir.output.corrected float_ops_main.cmx float_ops.output.exe)
 (deps float_ops.ml float_ops_main.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} float_ops.ml -c -llvm-backend -llvm-path ${OXCAML_CLANG} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to float_ops_ir.output.corrected (pipe-outputs (run cat float_ops.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} float_ops_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} float_ops.cmx float_ops_main.cmx -opaque -o float_ops.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps float_ops.cmx float_ops_main.cmx)
 (targets float_ops.output.corrected)
 (action
  (with-outputs-to
   float_ops.output.corrected
   (run ./float_ops.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps float_ops.output float_ops.output.corrected)
 (action
  (diff float_ops.output float_ops.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps float_ops_ir.output float_ops_ir.output.corrected)
 (action
  (diff float_ops_ir.output float_ops_ir.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (targets many_args_defn.cmx many_args.cmx many_args_ir.output.corrected many_args_main.cmx many_args.output.exe)
 (deps many_args_defn.ml many_args.ml many_args_main.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} many_args_defn.ml -c -llvm-backend -llvm-path ${OXCAML_CLANG} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} many_args.ml -c -llvm-backend -llvm-path ${OXCAML_CLANG} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to many_args_ir.output.corrected (pipe-outputs (run cat many_args.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} many_args_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} many_args_defn.cmx many_args.cmx many_args_main.cmx -opaque -o many_args.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps many_args_defn.cmx many_args.cmx many_args_main.cmx)
 (targets many_args.output.corrected)
 (action
  (with-outputs-to
   many_args.output.corrected
   (run ./many_args.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps many_args.output many_args.output.corrected)
 (action
  (diff many_args.output many_args.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps many_args_ir.output many_args_ir.output.corrected)
 (action
  (diff many_args_ir.output many_args_ir.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (targets multi_ret.cmx multi_ret_ir.output.corrected multi_ret_main.cmx multi_ret.output.exe)
 (deps multi_ret.ml multi_ret_main.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} multi_ret.ml -c -llvm-backend -llvm-path ${OXCAML_CLANG} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to multi_ret_ir.output.corrected (pipe-outputs (run cat multi_ret.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} multi_ret_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} multi_ret.cmx multi_ret_main.cmx -opaque -o multi_ret.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps multi_ret.cmx multi_ret_main.cmx)
 (targets multi_ret.output.corrected)
 (action
  (with-outputs-to
   multi_ret.output.corrected
   (run ./multi_ret.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps multi_ret.output multi_ret.output.corrected)
 (action
  (diff multi_ret.output multi_ret.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps multi_ret_ir.output multi_ret_ir.output.corrected)
 (action
  (diff multi_ret_ir.output multi_ret_ir.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (targets indirect_call.cmx indirect_call_ir.output.corrected indirect_call_main.cmx indirect_call.output.exe)
 (deps indirect_call.ml indirect_call_main.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} indirect_call.ml -c -llvm-backend -llvm-path ${OXCAML_CLANG} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to indirect_call_ir.output.corrected (pipe-outputs (run cat indirect_call.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} indirect_call_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} indirect_call.cmx indirect_call_main.cmx -opaque -o indirect_call.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps indirect_call.cmx indirect_call_main.cmx)
 (targets indirect_call.output.corrected)
 (action
  (with-outputs-to
   indirect_call.output.corrected
   (run ./indirect_call.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps indirect_call.output indirect_call.output.corrected)
 (action
  (diff indirect_call.output indirect_call.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps indirect_call_ir.output indirect_call_ir.output.corrected)
 (action
  (diff indirect_call_ir.output indirect_call_ir.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (targets extcalls_defn.o extcalls.cmx extcalls_ir.output.corrected extcalls_main.cmx extcalls.output.exe)
 (deps extcalls_defn.c extcalls.ml extcalls_main.ml)
 (action
  (progn
   (run clang extcalls_defn.c -c -g -O3 -I %{project_root}/runtime)
   (run %{bin:ocamlopt.opt} extcalls.ml -c -llvm-backend -llvm-path ${OXCAML_CLANG} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to extcalls_ir.output.corrected (pipe-outputs (run cat extcalls.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} extcalls_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} extcalls_defn.o extcalls.cmx extcalls_main.cmx -opaque -o extcalls.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps extcalls_defn.o extcalls.cmx extcalls_main.cmx)
 (targets extcalls.output.corrected)
 (action
  (with-outputs-to
   extcalls.output.corrected
   (run ./extcalls.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps extcalls.output extcalls.output.corrected)
 (action
  (diff extcalls.output extcalls.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps extcalls_ir.output extcalls_ir.output.corrected)
 (action
  (diff extcalls_ir.output extcalls_ir.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (targets data_decl.cmx data_decl_ir.output.corrected data_decl.output.exe)
 (deps data_decl.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} data_decl.ml -c -llvm-backend -llvm-path ${OXCAML_CLANG} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to data_decl_ir.output.corrected (pipe-outputs (run cat data_decl.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} data_decl.cmx -opaque -o data_decl.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps data_decl.cmx)
 (targets data_decl.output.corrected)
 (action
  (with-outputs-to
   data_decl.output.corrected
   (run ./data_decl.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps data_decl.output data_decl.output.corrected)
 (action
  (diff data_decl.output data_decl.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps data_decl_ir.output data_decl_ir.output.corrected)
 (action
  (diff data_decl_ir.output data_decl_ir.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (targets exn_part1.cmx exn_part2.cmx exn_part2_ir.output.corrected exn_part3.cmx exn.output.exe)
 (deps exn_part1.ml exn_part2.ml exn_part3.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} exn_part1.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} exn_part2.ml -c -llvm-backend -llvm-path ${OXCAML_CLANG} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to exn_part2_ir.output.corrected (pipe-outputs (run cat exn_part2.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} exn_part3.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} exn_part1.cmx exn_part2.cmx exn_part3.cmx -opaque -o exn.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps exn_part1.cmx exn_part2.cmx exn_part3.cmx)
 (targets exn.output.corrected)
 (action
  (with-outputs-to
   exn.output.corrected
   (run ./exn.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps exn.output exn.output.corrected)
 (action
  (diff exn.output exn.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps exn_part2_ir.output exn_part2_ir.output.corrected)
 (action
  (diff exn_part2_ir.output exn_part2_ir.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (targets alloc.cmx alloc_ir.output.corrected alloc.output.exe)
 (deps alloc.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} alloc.ml -c -llvm-backend -llvm-path ${OXCAML_CLANG} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to alloc_ir.output.corrected (pipe-outputs (run cat alloc.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} alloc.cmx -opaque -o alloc.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps alloc.cmx)
 (targets alloc.output.corrected)
 (action
  (with-outputs-to
   alloc.output.corrected
   (run ./alloc.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps alloc.output alloc.output.corrected)
 (action
  (diff alloc.output alloc.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps alloc_ir.output alloc_ir.output.corrected)
 (action
  (diff alloc_ir.output alloc_ir.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (targets tailcall2.cmx tailcall.cmx tailcall_ir.output.corrected tailcall.output.exe)
 (deps tailcall2.ml tailcall.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} tailcall2.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} tailcall.ml -c -llvm-backend -llvm-path ${OXCAML_CLANG} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to tailcall_ir.output.corrected (pipe-outputs (run cat tailcall.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} tailcall2.cmx tailcall.cmx -opaque -o tailcall.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps tailcall2.cmx tailcall.cmx)
 (targets tailcall.output.corrected)
 (action
  (with-outputs-to
   tailcall.output.corrected
   (run ./tailcall.output.exe))))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps tailcall.output tailcall.output.corrected)
 (action
  (diff tailcall.output tailcall.output.corrected)))

(rule
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{architecture} "amd64")
   (<> %{env:OXCAML_CLANG=} "")))
 (alias runtest-llvmize)
 (deps tailcall_ir.output tailcall_ir.output.corrected)
 (action
  (diff tailcall_ir.output tailcall_ir.output.corrected)))


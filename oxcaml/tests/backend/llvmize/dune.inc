(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets id_fn.output.corrected)
 (deps id_fn.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} id_fn.ml -c -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -dump-into-file -dcmm -dcfg -stop-after llvmize)
   (with-outputs-to id_fn.output.corrected (pipe-outputs (run cat id_fn.ll) (run ./filter.sh))))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps id_fn.output id_fn.output.corrected)
 (action
  (diff id_fn.output id_fn.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets const_val.cmx const_val_ir.output.corrected const_val_main.cmx const_val.output.exe)
 (deps const_val.ml const_val_main.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} const_val.ml -c -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to const_val_ir.output.corrected (pipe-outputs (run cat const_val.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} const_val_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} const_val.cmx const_val_main.cmx -opaque -o const_val.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps const_val.cmx const_val_main.cmx)
 (targets const_val.output.corrected)
 (action
  (with-outputs-to
   const_val.output.corrected
   (run ./const_val.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps const_val.output const_val.output.corrected)
 (action
  (diff const_val.output const_val.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps const_val_ir.output const_val_ir.output.corrected)
 (action
  (diff const_val_ir.output const_val_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets int_ops_data.cmx int_ops.cmx int_ops_ir.output.corrected int_ops_main.cmx int_ops.output.exe)
 (deps int_ops_data.ml int_ops.ml int_ops_main.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} int_ops_data.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} int_ops.ml -c -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to int_ops_ir.output.corrected (pipe-outputs (run cat int_ops.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} int_ops_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} int_ops_data.cmx int_ops.cmx int_ops_main.cmx -opaque -o int_ops.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps int_ops_data.cmx int_ops.cmx int_ops_main.cmx)
 (targets int_ops.output.corrected)
 (action
  (with-outputs-to
   int_ops.output.corrected
   (run ./int_ops.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps int_ops.output int_ops.output.corrected)
 (action
  (diff int_ops.output int_ops.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps int_ops_ir.output int_ops_ir.output.corrected)
 (action
  (diff int_ops_ir.output int_ops_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets gcd_data.cmx gcd.cmx gcd_ir.output.corrected gcd_main.cmx gcd.output.exe)
 (deps gcd_data.ml gcd.ml gcd_main.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} gcd_data.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} gcd.ml -c -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to gcd_ir.output.corrected (pipe-outputs (run cat gcd.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} gcd_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} gcd_data.cmx gcd.cmx gcd_main.cmx -opaque -o gcd.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps gcd_data.cmx gcd.cmx gcd_main.cmx)
 (targets gcd.output.corrected)
 (action
  (with-outputs-to
   gcd.output.corrected
   (run ./gcd.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps gcd.output gcd.output.corrected)
 (action
  (diff gcd.output gcd.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps gcd_ir.output gcd_ir.output.corrected)
 (action
  (diff gcd_ir.output gcd_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets array_rev_data.cmx array_rev.cmx array_rev_ir.output.corrected array_rev_main.cmx array_rev.output.exe)
 (deps array_rev_data.ml array_rev.ml array_rev_main.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} array_rev_data.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} array_rev.ml -c -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to array_rev_ir.output.corrected (pipe-outputs (run cat array_rev.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} array_rev_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} array_rev_data.cmx array_rev.cmx array_rev_main.cmx -opaque -o array_rev.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps array_rev_data.cmx array_rev.cmx array_rev_main.cmx)
 (targets array_rev.output.corrected)
 (action
  (with-outputs-to
   array_rev.output.corrected
   (run ./array_rev.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps array_rev.output array_rev.output.corrected)
 (action
  (diff array_rev.output array_rev.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps array_rev_ir.output array_rev_ir.output.corrected)
 (action
  (diff array_rev_ir.output array_rev_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets float_ops.cmx float_ops_ir.output.corrected float_ops_main.cmx float_ops.output.exe)
 (deps float_ops.ml float_ops_main.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} float_ops.ml -c -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to float_ops_ir.output.corrected (pipe-outputs (run cat float_ops.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} float_ops_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} float_ops.cmx float_ops_main.cmx -opaque -o float_ops.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps float_ops.cmx float_ops_main.cmx)
 (targets float_ops.output.corrected)
 (action
  (with-outputs-to
   float_ops.output.corrected
   (run ./float_ops.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps float_ops.output float_ops.output.corrected)
 (action
  (diff float_ops.output float_ops.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps float_ops_ir.output float_ops_ir.output.corrected)
 (action
  (diff float_ops_ir.output float_ops_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets many_args_defn.cmx many_args.cmx many_args_ir.output.corrected many_args_main.cmx many_args.output.exe)
 (deps many_args_defn.ml many_args.ml many_args_main.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} many_args_defn.ml -c -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} many_args.ml -c -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to many_args_ir.output.corrected (pipe-outputs (run cat many_args.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} many_args_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} many_args_defn.cmx many_args.cmx many_args_main.cmx -opaque -o many_args.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps many_args_defn.cmx many_args.cmx many_args_main.cmx)
 (targets many_args.output.corrected)
 (action
  (with-outputs-to
   many_args.output.corrected
   (run ./many_args.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps many_args.output many_args.output.corrected)
 (action
  (diff many_args.output many_args.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps many_args_ir.output many_args_ir.output.corrected)
 (action
  (diff many_args_ir.output many_args_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets multi_ret.cmx multi_ret_ir.output.corrected multi_ret_main.cmx multi_ret.output.exe)
 (deps multi_ret.ml multi_ret_main.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} multi_ret.ml -c -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to multi_ret_ir.output.corrected (pipe-outputs (run cat multi_ret.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} multi_ret_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} multi_ret.cmx multi_ret_main.cmx -opaque -o multi_ret.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps multi_ret.cmx multi_ret_main.cmx)
 (targets multi_ret.output.corrected)
 (action
  (with-outputs-to
   multi_ret.output.corrected
   (run ./multi_ret.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps multi_ret.output multi_ret.output.corrected)
 (action
  (diff multi_ret.output multi_ret.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps multi_ret_ir.output multi_ret_ir.output.corrected)
 (action
  (diff multi_ret_ir.output multi_ret_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets indirect_call.cmx indirect_call_ir.output.corrected indirect_call_main.cmx indirect_call.output.exe)
 (deps indirect_call.ml indirect_call_main.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} indirect_call.ml -c -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to indirect_call_ir.output.corrected (pipe-outputs (run cat indirect_call.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} indirect_call_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} indirect_call.cmx indirect_call_main.cmx -opaque -o indirect_call.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps indirect_call.cmx indirect_call_main.cmx)
 (targets indirect_call.output.corrected)
 (action
  (with-outputs-to
   indirect_call.output.corrected
   (run ./indirect_call.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps indirect_call.output indirect_call.output.corrected)
 (action
  (diff indirect_call.output indirect_call.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps indirect_call_ir.output indirect_call_ir.output.corrected)
 (action
  (diff indirect_call_ir.output indirect_call_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets extcalls_defn.o extcalls.cmx extcalls_ir.output.corrected extcalls_main.cmx extcalls.output.exe)
 (deps extcalls_defn.c extcalls.ml extcalls_main.ml)
 (action
  (progn
   (run clang extcalls_defn.c -c -g -O3 -I %{project_root}/runtime)
   (run %{bin:ocamlopt.opt} extcalls.ml -c -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (with-outputs-to extcalls_ir.output.corrected (pipe-outputs (run cat extcalls.ll) (run ./filter.sh)))
   (run %{bin:ocamlopt.opt} extcalls_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} extcalls_defn.o extcalls.cmx extcalls_main.cmx -opaque -o extcalls.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps extcalls_defn.o extcalls.cmx extcalls_main.cmx)
 (targets extcalls.output.corrected)
 (action
  (with-outputs-to
   extcalls.output.corrected
   (run ./extcalls.output.exe))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps extcalls.output extcalls.output.corrected)
 (action
  (diff extcalls.output extcalls.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps extcalls_ir.output extcalls_ir.output.corrected)
 (action
  (diff extcalls_ir.output extcalls_ir.output.corrected)))


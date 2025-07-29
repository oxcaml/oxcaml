
(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets id_fn.output.corrected)
 (deps id_fn.ml filter.sh)
 (action
  (with-outputs-to
   id_fn.output.corrected
   (pipe-outputs
    (run
     %{bin:ocamlopt.opt} id_fn.ml -g -c -O3 -llvm-backend -stop-after llvmize -keep-llvmir -dno-asm-comments)
    (run cat id_fn.ll)
    (run ./filter.sh)))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest)
 (deps id_fn.output id_fn.output.corrected)
 (action
  (diff id_fn.output id_fn.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets const_val.output.exe const_val_ir.output.corrected)
 (deps  const_val_main.ml const_val.ml)
 (action
  (progn
   
   (run %{bin:ocamlopt.opt} const_val.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion)
   (run %{bin:ocamlopt.opt} const_val_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt}  const_val.cmx const_val_main.cmx -opaque -o const_val.output.exe)
   (with-outputs-to
    const_val_ir.output.corrected
    (pipe-outputs
      (run cat const_val.ll)
      (run ./filter.sh))))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps const_val_ir.output const_val_ir.output.corrected)
 (action
  (diff const_val_ir.output const_val_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (deps const_val.output.exe)
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
 (targets int_ops.output.exe int_ops_ir.output.corrected)
 (deps int_ops_data.ml int_ops_main.ml int_ops.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} int_ops_data.ml -g -c -O3 -opaque )
   (run %{bin:ocamlopt.opt} int_ops.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion)
   (run %{bin:ocamlopt.opt} int_ops_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} int_ops_data.cmx int_ops.cmx int_ops_main.cmx -opaque -o int_ops.output.exe)
   (with-outputs-to
    int_ops_ir.output.corrected
    (pipe-outputs
      (run cat int_ops.ll)
      (run ./filter.sh))))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps int_ops_ir.output int_ops_ir.output.corrected)
 (action
  (diff int_ops_ir.output int_ops_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (deps int_ops.output.exe)
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
 (targets gcd.output.exe gcd_ir.output.corrected)
 (deps gcd_data.ml gcd_main.ml gcd.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} gcd_data.ml -g -c -O3 -opaque )
   (run %{bin:ocamlopt.opt} gcd.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion)
   (run %{bin:ocamlopt.opt} gcd_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} gcd_data.cmx gcd.cmx gcd_main.cmx -opaque -o gcd.output.exe)
   (with-outputs-to
    gcd_ir.output.corrected
    (pipe-outputs
      (run cat gcd.ll)
      (run ./filter.sh))))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps gcd_ir.output gcd_ir.output.corrected)
 (action
  (diff gcd_ir.output gcd_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (deps gcd.output.exe)
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
 (targets array_rev.output.exe array_rev_ir.output.corrected)
 (deps array_rev_data.ml array_rev_main.ml array_rev.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} array_rev_data.ml -g -c -O3 -opaque )
   (run %{bin:ocamlopt.opt} array_rev.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion)
   (run %{bin:ocamlopt.opt} array_rev_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} array_rev_data.cmx array_rev.cmx array_rev_main.cmx -opaque -o array_rev.output.exe)
   (with-outputs-to
    array_rev_ir.output.corrected
    (pipe-outputs
      (run cat array_rev.ll)
      (run ./filter.sh))))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps array_rev_ir.output array_rev_ir.output.corrected)
 (action
  (diff array_rev_ir.output array_rev_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (deps array_rev.output.exe)
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
 (targets float_ops.output.exe float_ops_ir.output.corrected)
 (deps  float_ops_main.ml float_ops.ml)
 (action
  (progn
   
   (run %{bin:ocamlopt.opt} float_ops.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion)
   (run %{bin:ocamlopt.opt} float_ops_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt}  float_ops.cmx float_ops_main.cmx -opaque -o float_ops.output.exe)
   (with-outputs-to
    float_ops_ir.output.corrected
    (pipe-outputs
      (run cat float_ops.ll)
      (run ./filter.sh))))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps float_ops_ir.output float_ops_ir.output.corrected)
 (action
  (diff float_ops_ir.output float_ops_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (deps float_ops.output.exe)
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
 (targets many_args.output.exe many_args_ir.output.corrected)
 (deps many_args_defn.ml many_args_main.ml many_args.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} many_args_defn.ml -g -c -O3 -opaque -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion)
   (run %{bin:ocamlopt.opt} many_args.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion)
   (run %{bin:ocamlopt.opt} many_args_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} many_args_defn.cmx many_args.cmx many_args_main.cmx -opaque -o many_args.output.exe)
   (with-outputs-to
    many_args_ir.output.corrected
    (pipe-outputs
      (run cat many_args.ll)
      (run ./filter.sh))))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps many_args_ir.output many_args_ir.output.corrected)
 (action
  (diff many_args_ir.output many_args_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (deps many_args.output.exe)
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
 (targets multi_ret.output.exe multi_ret_ir.output.corrected)
 (deps  multi_ret_main.ml multi_ret.ml)
 (action
  (progn
   
   (run %{bin:ocamlopt.opt} multi_ret.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion)
   (run %{bin:ocamlopt.opt} multi_ret_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt}  multi_ret.cmx multi_ret_main.cmx -opaque -o multi_ret.output.exe)
   (with-outputs-to
    multi_ret_ir.output.corrected
    (pipe-outputs
      (run cat multi_ret.ll)
      (run ./filter.sh))))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps multi_ret_ir.output multi_ret_ir.output.corrected)
 (action
  (diff multi_ret_ir.output multi_ret_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (deps multi_ret.output.exe)
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
 (targets indirect_call.output.exe indirect_call_ir.output.corrected)
 (deps  indirect_call_main.ml indirect_call.ml)
 (action
  (progn
   
   (run %{bin:ocamlopt.opt} indirect_call.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear -llvm-backend -llvm-path ${LLVM_PATH:-clang} -keep-llvmir -dno-asm-comments -disable-poll-insertion)
   (run %{bin:ocamlopt.opt} indirect_call_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt}  indirect_call.cmx indirect_call_main.cmx -opaque -o indirect_call.output.exe)
   (with-outputs-to
    indirect_call_ir.output.corrected
    (pipe-outputs
      (run cat indirect_call.ll)
      (run ./filter.sh))))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest-llvm)
 (deps indirect_call_ir.output indirect_call_ir.output.corrected)
 (action
  (diff indirect_call_ir.output indirect_call_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (deps indirect_call.output.exe)
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

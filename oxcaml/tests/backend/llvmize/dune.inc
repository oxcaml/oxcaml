
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
   
   (run %{bin:ocamlopt.opt} const_val.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear -llvm-backend -llvm-path clang -keep-llvmir -dno-asm-comments -disable-poll-insertion)
   (run %{bin:ocamlopt.opt} const_val_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt}  const_val.cmx const_val_main.cmx -opaque -o const_val.output.exe)
   (run mv const_val.ll const_val_ir.output.corrected))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest)
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
 (alias runtest)
 (deps const_val.output const_val.output.corrected)
 (action
  (diff const_val.output const_val.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets int_ops.output.exe int_ops_ir.output.corrected)
 (deps int_ops_data.ml int_ops_main.ml int_ops.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} int_ops_data.ml -g -c -O3 -opaque)
   (run %{bin:ocamlopt.opt} int_ops.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear -llvm-backend -llvm-path clang -keep-llvmir -dno-asm-comments -disable-poll-insertion)
   (run %{bin:ocamlopt.opt} int_ops_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} int_ops_data.cmx int_ops.cmx int_ops_main.cmx -opaque -o int_ops.output.exe)
   (run mv int_ops.ll int_ops_ir.output.corrected))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest)
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
 (alias runtest)
 (deps int_ops.output int_ops.output.corrected)
 (action
  (diff int_ops.output int_ops.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets gcd.output.exe gcd_ir.output.corrected)
 (deps gcd_data.ml gcd_main.ml gcd.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} gcd_data.ml -g -c -O3 -opaque)
   (run %{bin:ocamlopt.opt} gcd.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear -llvm-backend -llvm-path clang -keep-llvmir -dno-asm-comments -disable-poll-insertion)
   (run %{bin:ocamlopt.opt} gcd_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} gcd_data.cmx gcd.cmx gcd_main.cmx -opaque -o gcd.output.exe)
   (run mv gcd.ll gcd_ir.output.corrected))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest)
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
 (alias runtest)
 (deps gcd.output gcd.output.corrected)
 (action
  (diff gcd.output gcd.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets array_rev.output.exe array_rev_ir.output.corrected)
 (deps array_rev_data.ml array_rev_main.ml array_rev.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} array_rev_data.ml -g -c -O3 -opaque)
   (run %{bin:ocamlopt.opt} array_rev.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear -llvm-backend -llvm-path clang -keep-llvmir -dno-asm-comments -disable-poll-insertion)
   (run %{bin:ocamlopt.opt} array_rev_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt} array_rev_data.cmx array_rev.cmx array_rev_main.cmx -opaque -o array_rev.output.exe)
   (run mv array_rev.ll array_rev_ir.output.corrected))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest)
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
 (alias runtest)
 (deps array_rev.output array_rev.output.corrected)
 (action
  (diff array_rev.output array_rev.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets float_ops.output.exe float_ops_ir.output.corrected)
 (deps  float_ops_main.ml float_ops.ml)
 (action
  (progn
   
   (run %{bin:ocamlopt.opt} float_ops.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear -llvm-backend -llvm-path clang -keep-llvmir -dno-asm-comments -disable-poll-insertion)
   (run %{bin:ocamlopt.opt} float_ops_main.ml -c -g -O3 -opaque -S -dump-into-file -dcmm -dcfg -dlinear)
   (run %{bin:ocamlopt.opt}  float_ops.cmx float_ops_main.cmx -opaque -o float_ops.output.exe)
   (run mv float_ops.ll float_ops_ir.output.corrected))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest)
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
 (alias runtest)
 (deps float_ops.output float_ops.output.corrected)
 (action
  (diff float_ops.output float_ops.output.corrected)))

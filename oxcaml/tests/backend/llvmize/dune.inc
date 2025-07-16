
(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets id_fn_ir.output.corrected)
 (deps id_fn_ir.ml filter.sh)
 (action
  (with-outputs-to
   id_fn_ir.output.corrected
   (pipe-outputs
    (run
     %{bin:ocamlopt.opt} id_fn_ir.ml -g -c -O3 -llvm-backend -stop-after llvmize -keep-llvmir)
    (run cat id_fn_ir.ll)
    (run ./filter.sh)))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest)
 (deps id_fn_ir.output id_fn_ir.output.corrected)
 (action
  (diff id_fn_ir.output id_fn_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets const_val_ir.output.corrected)
 (deps const_val_ir.ml filter.sh)
 (action
  (with-outputs-to
   const_val_ir.output.corrected
   (pipe-outputs
    (run
     %{bin:ocamlopt.opt} const_val_ir.ml -g -c -O3 -llvm-backend -stop-after llvmize -keep-llvmir)
    (run cat const_val_ir.ll)
    (run ./filter.sh)))))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (alias runtest)
 (deps const_val_ir.output const_val_ir.output.corrected)
 (action
  (diff const_val_ir.output const_val_ir.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets const_val.output.exe)
 (deps const_val_main.ml const_val_test.ml)
 (action
  (progn
   (run %{bin:ocamlopt.opt} const_val_test.ml -c -g -O3 -opaque -llvm-backend -llvm-path clang -dno-asm-comments)
   (run %{bin:ocamlopt.opt} const_val_main.ml -c -g -O3 -opaque)
   (run %{bin:ocamlopt.opt} const_val_test.cmx const_val_main.cmx -opaque -o const_val.output.exe))))

(rule
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

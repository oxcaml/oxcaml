
(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")))
 (targets id_fn.output.corrected)
 (deps (:ml id_fn.ml) filter.sh)
 (action
  (with-outputs-to
   id_fn.output.corrected
   (pipe-outputs
    (run
     %{bin:ocamlopt.opt} %{ml} -c -llvm-backend -stop-after llvmize -keep-llvmir)
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
 (targets const_val.output.exe)
 (deps (:ml const_val_bootstrap.ml const_val.ml))
 (action
  (pipe-outputs
   (run %{bin:ocamlopt.opt} const_val.ml -c -opaque -llvm-backend -llvm-path clang)
   (run %{bin:ocamlopt.opt} const_val_bootstrap.ml -c -opaque)
   (run %{bin:ocamlopt.opt} const_val.cmx const_val_bootstrap.cmx -opaque -o const_val.output.exe))))

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

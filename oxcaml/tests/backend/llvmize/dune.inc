
(rule
(enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets id_fn.output.corrected)
 (deps (:ml id_fn.ml) filter.sh)
 (action
  (with-outputs-to
   id_fn.output.corrected
   (pipe-outputs
    (run
     %{bin:ocamlopt.opt} %{ml} -g -c -g -c -llvm-backend -llvm-path true -dllvmir)
    (run cat id_fn.ll)
    (run filter.sh)))))

(rule
(enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (alias runtest)
 (deps id_fn.output id_fn.output.corrected)
 (action
  (diff id_fn.output id_fn.output.corrected)))

(rule
(enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets const_val.output.corrected)
 (deps const_val_bootstrap (:ml const_val.ml))
 (action
  (with-outputs-to
   const_val.output.corrected
   (pipe-outputs
    (run
     %{bin:ocamlopt.opt} const_val.ml -c -opaque -g -c -llvm-backend -llvm-path clang)
    (run
     %{bin:ocamlopt.opt} const_val_bootstrap.ml -c -opaque)
    (run
     %{bin:ocamlopt.opt} const_val.cmx const_val_bootstrap.cmx -opaque -o test.exe)
    (run ./test.exe)))))

(rule
(enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (alias runtest)
 (deps const_val.output const_val.output.corrected)
 (action
  (diff const_val.output const_val.output.corrected)))

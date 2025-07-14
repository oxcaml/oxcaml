
(rule
 (enabled_if  (= %{context_name} "main"))
 (targets id_fn.output.corrected)
 (deps (:ml id_fn.ml) filter.sh)
 (action
  (pipe-outputs
    (with-accepted-exit-codes 0
    (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
         -llvm-backend -llvm-path true -dllvm
         -zero-alloc-check default))
    (run mv id_fn.ll id_fn.output.corrected)
    (run "./filter.sh")
   )))

(rule
 (alias   runtest)
 (enabled_if  (= %{context_name} "main"))
 (deps id_fn.output id_fn.output.corrected)
 (action (diff id_fn.output id_fn.output.corrected)))


(rule
 (targets id_fn.output.corrected)
 (deps (:ml id_fn.ml) filter.sh)
 (action
  (with-outputs-to
   id_fn.output.corrected
   (pipe-outputs
    (with-accepted-exit-codes 0
     (run
      %{bin:boot_ocamlopt} %{ml} -g -c
      -llvm-backend -llvm-path true -dllvmir
      ))
    (run cat id_fn.ll)
    (run ./filter.sh)))))

(rule
 (alias runtest)
 (deps id_fn.output id_fn.output.corrected)
 (action
  (diff id_fn.output id_fn.output.corrected)))

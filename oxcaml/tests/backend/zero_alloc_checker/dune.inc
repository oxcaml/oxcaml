
(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (deps s.ml t.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dzero-alloc -dump-into-file -O3 -warn-error +a)))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (deps test_flambda2_invalid.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dzero-alloc -dump-into-file -O3 -warn-error +a)))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (deps t5.ml test_assume.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dzero-alloc -dump-into-file -O3 -warn-error +a)))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (deps test_match_on_mutable_state.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dzero-alloc -dump-into-file -O3 -warn-error +a)))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (deps test_flambda.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dzero-alloc -dump-into-file -O3 -warn-error +a)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail1.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail1.ml) filter.sh)
 (action
   (with-outputs-to fail1.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail1.%{env:OXCAML_NAME_MANGLING=flat}.output fail1.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail2.ml) filter.sh)
 (action
   (with-outputs-to fail2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail2.%{env:OXCAML_NAME_MANGLING=flat}.output fail2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail3.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml t3.ml fail3.ml) filter.sh)
 (action
   (with-outputs-to fail3.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail3.%{env:OXCAML_NAME_MANGLING=flat}.output fail3.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail4.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml t4.ml fail4.ml) filter.sh)
 (action
   (with-outputs-to fail4.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail4.%{env:OXCAML_NAME_MANGLING=flat}.output fail4.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail5.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail5.ml) filter.sh)
 (action
   (with-outputs-to fail5.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail5.%{env:OXCAML_NAME_MANGLING=flat}.output fail5.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail6.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail6.ml) filter.sh)
 (action
   (with-outputs-to fail6.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail6.%{env:OXCAML_NAME_MANGLING=flat}.output fail6.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail7.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail7.ml) filter.sh)
 (action
   (with-outputs-to fail7.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail7.%{env:OXCAML_NAME_MANGLING=flat}.output fail7.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail8.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail8.ml) filter.sh)
 (action
   (with-outputs-to fail8.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 0
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail8.%{env:OXCAML_NAME_MANGLING=flat}.output fail8.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail9.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail9.ml) filter.sh)
 (action
   (with-outputs-to fail9.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail9.%{env:OXCAML_NAME_MANGLING=flat}.output fail9.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail10.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail10.ml) filter.sh)
 (action
   (with-outputs-to fail10.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail10.%{env:OXCAML_NAME_MANGLING=flat}.output fail10.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail12.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail12.ml) filter.sh)
 (action
   (with-outputs-to fail12.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail12.%{env:OXCAML_NAME_MANGLING=flat}.output fail12.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail13.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail13.ml) filter.sh)
 (action
   (with-outputs-to fail13.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail13.%{env:OXCAML_NAME_MANGLING=flat}.output fail13.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail14.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail14.ml) filter.sh)
 (action
   (with-outputs-to fail14.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail14.%{env:OXCAML_NAME_MANGLING=flat}.output fail14.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail15.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail15.ml) filter.sh)
 (action
   (with-outputs-to fail15.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail15.%{env:OXCAML_NAME_MANGLING=flat}.output fail15.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail16.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail16.ml) filter.sh)
 (action
   (with-outputs-to fail16.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail16.%{env:OXCAML_NAME_MANGLING=flat}.output fail16.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail17.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail17.ml) filter.sh)
 (action
   (with-outputs-to fail17.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail17.%{env:OXCAML_NAME_MANGLING=flat}.output fail17.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail18.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail18.ml) filter.sh)
 (action
   (with-outputs-to fail18.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail18.%{env:OXCAML_NAME_MANGLING=flat}.output fail18.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail19.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml dep19.ml fail19.ml) filter.sh)
 (action
   (with-outputs-to fail19.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail19.%{env:OXCAML_NAME_MANGLING=flat}.output fail19.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail20.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail20.ml) filter.sh)
 (action
   (with-outputs-to fail20.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail20.%{env:OXCAML_NAME_MANGLING=flat}.output fail20.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail21.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail21.ml) filter.sh)
 (action
   (with-outputs-to fail21.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail21.%{env:OXCAML_NAME_MANGLING=flat}.output fail21.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_attribute_error_duplicate.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_attribute_error_duplicate.ml) filter.sh)
 (action
   (with-outputs-to test_attribute_error_duplicate.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 0 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_attribute_error_duplicate.%{env:OXCAML_NAME_MANGLING=flat}.output test_attribute_error_duplicate.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_attr_unused.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_attr_unused.ml) filter.sh)
 (action
   (with-outputs-to test_attr_unused.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_attr_unused.%{env:OXCAML_NAME_MANGLING=flat}.output test_attr_unused.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets t6.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml t6.ml) filter.sh)
 (action
   (with-outputs-to t6.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 0
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff t6.%{env:OXCAML_NAME_MANGLING=flat}.output t6.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (deps t7.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dzero-alloc -dump-into-file -O3 -warn-error +a)))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (deps test_stub_dep.ml test_stub.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dzero-alloc -dump-into-file -O3 -warn-error +a)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets t1.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml t1.ml) filter.sh)
 (action
   (with-outputs-to t1.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff t1.%{env:OXCAML_NAME_MANGLING=flat}.output t1.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_warning199.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_warning199.mli test_warning199.ml) filter.sh)
 (action
   (with-outputs-to test_warning199.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 0
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_warning199.%{env:OXCAML_NAME_MANGLING=flat}.output test_warning199.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_warning199_disabled.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_warning199_disabled.mli test_warning199_disabled.ml) filter.sh)
 (action
   (with-outputs-to test_warning199_disabled.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 0
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_warning199_disabled.%{env:OXCAML_NAME_MANGLING=flat}.output test_warning199_disabled.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_never_returns_normally.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_never_returns_normally.ml) filter.sh)
 (action
   (with-outputs-to test_never_returns_normally.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 0
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_never_returns_normally.%{env:OXCAML_NAME_MANGLING=flat}.output test_never_returns_normally.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail22.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail22.ml) filter.sh)
 (action
   (with-outputs-to fail22.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check opt -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail22.%{env:OXCAML_NAME_MANGLING=flat}.output fail22.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail23.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail23.ml) filter.sh)
 (action
   (with-outputs-to fail23.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check opt -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail23.%{env:OXCAML_NAME_MANGLING=flat}.output fail23.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (deps test_zero_alloc_opt1.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check opt -dcse -dzero-alloc -dump-into-file -O3 -warn-error +a)))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (deps test_zero_alloc_opt2.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check opt -dcse -dzero-alloc -dump-into-file -O3 -warn-error +a)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_assume_fail.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_assume_fail.ml) filter.sh)
 (action
   (with-outputs-to test_assume_fail.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -directory repo/root/relative/dir -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_assume_fail.%{env:OXCAML_NAME_MANGLING=flat}.output test_assume_fail.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_assume_on_call.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_assume_on_call.ml) filter.sh)
 (action
   (with-outputs-to test_assume_on_call.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -no-zero-alloc-checker-details-extra -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_assume_on_call.%{env:OXCAML_NAME_MANGLING=flat}.output test_assume_on_call.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_misplaced_assume.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_misplaced_assume.ml) filter.sh)
 (action
   (with-outputs-to test_misplaced_assume.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_misplaced_assume.%{env:OXCAML_NAME_MANGLING=flat}.output test_misplaced_assume.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_attr_check.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_attr_check.ml) filter.sh)
 (action
   (with-outputs-to test_attr_check.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check all -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_attr_check.%{env:OXCAML_NAME_MANGLING=flat}.output test_attr_check.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_attr_check_all.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_attr_check_all.ml) filter.sh)
 (action
   (with-outputs-to test_attr_check_all.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_attr_check_all.%{env:OXCAML_NAME_MANGLING=flat}.output test_attr_check_all.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_attr_check_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_attr_check_opt.ml) filter.sh)
 (action
   (with-outputs-to test_attr_check_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_attr_check_opt.%{env:OXCAML_NAME_MANGLING=flat}.output test_attr_check_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_attr_check_none.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_attr_check_none.ml) filter.sh)
 (action
   (with-outputs-to test_attr_check_none.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 0
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_attr_check_none.%{env:OXCAML_NAME_MANGLING=flat}.output test_attr_check_none.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail24.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail24.ml) filter.sh)
 (action
   (with-outputs-to fail24.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail24.%{env:OXCAML_NAME_MANGLING=flat}.output fail24.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (deps test_raise_message.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -function-layout topological -dcse -dzero-alloc -dump-into-file -O3 -warn-error +a)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail25.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail25.ml) filter.sh)
 (action
   (with-outputs-to fail25.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -disable-precise-zero-alloc-checker -function-layout source -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail25.%{env:OXCAML_NAME_MANGLING=flat}.output fail25.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail26.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail26.ml) filter.sh)
 (action
   (with-outputs-to fail26.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -disable-zero-alloc-checker -function-layout source -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail26.%{env:OXCAML_NAME_MANGLING=flat}.output fail26.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_all_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_all_opt.ml) filter.sh)
 (action
   (with-outputs-to test_all_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_all_opt.%{env:OXCAML_NAME_MANGLING=flat}.output test_all_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_all_opt2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_all_opt2.ml) filter.sh)
 (action
   (with-outputs-to test_all_opt2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check all -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_all_opt2.%{env:OXCAML_NAME_MANGLING=flat}.output test_all_opt2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_all_opt3.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_all_opt3.ml) filter.sh)
 (action
   (with-outputs-to test_all_opt3.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check opt -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_all_opt3.%{env:OXCAML_NAME_MANGLING=flat}.output test_all_opt3.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_arity.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_arity.ml) filter.sh)
 (action
   (with-outputs-to test_arity.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_arity.%{env:OXCAML_NAME_MANGLING=flat}.output test_arity.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (alias runtest)
 (deps stop_after_typing.ml)
 (target stop_after_typing.cmi)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (run %{bin:ocamlopt.opt} stop_after_typing.ml -g -c -opaque -stop-after typing -O3 -warn-error +a)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_signatures_functors.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_signatures_functors.ml) filter.sh)
 (action
   (with-outputs-to test_signatures_functors.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_signatures_functors.%{env:OXCAML_NAME_MANGLING=flat}.output test_signatures_functors.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_signatures_first_class_modules.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_signatures_first_class_modules.ml) filter.sh)
 (action
   (with-outputs-to test_signatures_first_class_modules.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_signatures_first_class_modules.%{env:OXCAML_NAME_MANGLING=flat}.output test_signatures_first_class_modules.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (alias runtest)
 (deps test_signatures_separate_a.ml)
 (target test_signatures_separate_a.cmi)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (run %{bin:ocamlopt.opt} test_signatures_separate_a.ml -g -c -opaque -stop-after typing -O3 -warn-error +a)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_signatures_separate.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps test_signatures_separate_a.cmi (:ml test_signatures_separate_b.ml) filter.sh)
 (action
   (with-outputs-to test_signatures_separate.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_signatures_separate.%{env:OXCAML_NAME_MANGLING=flat}.output test_signatures_separate.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_signatures_separate.opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps test_signatures_separate_a.cmi (:ml test_signatures_separate_b.ml) filter.sh)
 (action
   (with-outputs-to test_signatures_separate.opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check all -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_signatures_separate.opt.%{env:OXCAML_NAME_MANGLING=flat}.output test_signatures_separate.opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_assume_inlining.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_assume_inlining.ml) filter.sh)
 (action
   (with-outputs-to test_assume_inlining.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_assume_inlining.%{env:OXCAML_NAME_MANGLING=flat}.output test_assume_inlining.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_assume_error.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_assume_error.ml) filter.sh)
 (action
   (with-outputs-to test_assume_error.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_assume_error.%{env:OXCAML_NAME_MANGLING=flat}.output test_assume_error.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_assume_stub.output.corrected)
 (deps (:ml test_assume_stub.ml) filter.sh)
 (action
   (with-outputs-to test_assume_stub.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_assume_stub.output test_assume_stub.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_bounded_join.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_bounded_join.ml) filter_fatal_error.sh)
 (action
   (with-outputs-to test_bounded_join.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-join -2 -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter_fatal_error.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_bounded_join.%{env:OXCAML_NAME_MANGLING=flat}.output test_bounded_join.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_bounded_join2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_bounded_join2.ml) filter.sh)
 (action
   (with-outputs-to test_bounded_join2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-join 2 -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_bounded_join2.%{env:OXCAML_NAME_MANGLING=flat}.output test_bounded_join2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_bounded_join3.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_bounded_join3.ml) filter.sh)
 (action
   (with-outputs-to test_bounded_join3.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-join 0 -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_bounded_join3.%{env:OXCAML_NAME_MANGLING=flat}.output test_bounded_join3.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_bounded_join4.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_bounded_join4.ml) filter.sh)
 (action
   (with-outputs-to test_bounded_join4.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-join 0 -zero-alloc-checker-details-cutoff 3 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_bounded_join4.%{env:OXCAML_NAME_MANGLING=flat}.output test_bounded_join4.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_inference.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_inference.mli test_inference.ml) filter.sh)
 (action
   (with-outputs-to test_inference.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_inference.%{env:OXCAML_NAME_MANGLING=flat}.output test_inference.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_inference.opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_inference.mli test_inference.ml) filter.sh)
 (action
   (with-outputs-to test_inference.opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check all -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_inference.opt.%{env:OXCAML_NAME_MANGLING=flat}.output test_inference.opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_remove_inferred_assume.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_remove_inferred_assume.ml) filter.sh)
 (action
   (with-outputs-to test_remove_inferred_assume.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_remove_inferred_assume.%{env:OXCAML_NAME_MANGLING=flat}.output test_remove_inferred_assume.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (deps test_remove_inferred_assume_workaround.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dzero-alloc -dump-into-file -O3 -warn-error +a)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail27.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail27.ml) filter.sh)
 (action
   (with-outputs-to fail27.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-assert all -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail27.%{env:OXCAML_NAME_MANGLING=flat}.output fail27.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail27_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail27_opt.ml) filter.sh)
 (action
   (with-outputs-to fail27_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-assert all_opt -zero-alloc-check all -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail27_opt.%{env:OXCAML_NAME_MANGLING=flat}.output fail27_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail27_default.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail27_default.ml) filter.sh)
 (action
   (with-outputs-to fail27_default.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-assert default -zero-alloc-check all -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail27_default.%{env:OXCAML_NAME_MANGLING=flat}.output fail27_default.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail28.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail28.ml) filter.sh)
 (action
   (with-outputs-to fail28.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail28.%{env:OXCAML_NAME_MANGLING=flat}.output fail28.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail28_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml fail28_opt.ml) filter.sh)
 (action
   (with-outputs-to fail28_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check all -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail28_opt.%{env:OXCAML_NAME_MANGLING=flat}.output fail28_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (alias runtest)
 (deps fail29.mli)
 (target fail29.cmi)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (run %{bin:ocamlopt.opt} fail29.mli -g -c -opaque -stop-after typing -O3 -warn-error +a)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail29.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps fail29.cmi (:ml fail29.ml) filter.sh)
 (action
   (with-outputs-to fail29.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail29.%{env:OXCAML_NAME_MANGLING=flat}.output fail29.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (alias runtest)
 (deps fail29_opt.mli)
 (target fail29_opt.cmi)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (run %{bin:ocamlopt.opt} fail29_opt.mli -g -c -opaque -stop-after typing -O3 -warn-error +a)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail29_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps fail29_opt.cmi (:ml fail29_opt.ml) filter.sh)
 (action
   (with-outputs-to fail29_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail29_opt.%{env:OXCAML_NAME_MANGLING=flat}.output fail29_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (alias runtest)
 (deps fail29_opt2.mli)
 (target fail29_opt2.cmi)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (run %{bin:ocamlopt.opt} fail29_opt2.mli -g -c -opaque -stop-after typing -O3 -warn-error +a)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets fail29_opt2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps fail29_opt2.cmi (:ml fail29_opt2.ml) filter.sh)
 (action
   (with-outputs-to fail29_opt2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check all -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff fail29_opt2.%{env:OXCAML_NAME_MANGLING=flat}.output fail29_opt2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_assume_unless_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_assume_unless_opt.ml) filter.sh)
 (action
   (with-outputs-to test_assume_unless_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check all -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_assume_unless_opt.%{env:OXCAML_NAME_MANGLING=flat}.output test_assume_unless_opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (deps test_assume_unless_opt.ml)
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c -zero-alloc-check default -dcse -dzero-alloc -dump-into-file -O3 -warn-error +a)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_assume_unless_opt_sig.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_assume_unless_opt_sig.ml) filter.sh)
 (action
   (with-outputs-to test_assume_unless_opt_sig.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 0
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_assume_unless_opt_sig.%{env:OXCAML_NAME_MANGLING=flat}.output test_assume_unless_opt_sig.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_assume_unless_opt_sig2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_assume_unless_opt_sig2.ml) filter.sh)
 (action
   (with-outputs-to test_assume_unless_opt_sig2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check all -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_assume_unless_opt_sig2.%{env:OXCAML_NAME_MANGLING=flat}.output test_assume_unless_opt_sig2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_assume_unless_opt_on_call.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_assume_unless_opt_on_call.ml) filter.sh)
 (action
   (with-outputs-to test_assume_unless_opt_on_call.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_assume_unless_opt_on_call.%{env:OXCAML_NAME_MANGLING=flat}.output test_assume_unless_opt_on_call.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_assume_unless_opt_on_call2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_assume_unless_opt_on_call2.ml) filter.sh)
 (action
   (with-outputs-to test_assume_unless_opt_on_call2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check all -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_assume_unless_opt_on_call2.%{env:OXCAML_NAME_MANGLING=flat}.output test_assume_unless_opt_on_call2.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_custom_error_msg.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_custom_error_msg.ml) filter.sh)
 (action
   (with-outputs-to test_custom_error_msg.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_custom_error_msg.%{env:OXCAML_NAME_MANGLING=flat}.output test_custom_error_msg.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_custom_error_msg.opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_custom_error_msg.ml) filter.sh)
 (action
   (with-outputs-to test_custom_error_msg.opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check all -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_custom_error_msg.opt.%{env:OXCAML_NAME_MANGLING=flat}.output test_custom_error_msg.opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_custom_error_msg_sig.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_custom_error_msg_sig.mli test_custom_error_msg_sig.ml) filter.sh)
 (action
   (with-outputs-to test_custom_error_msg_sig.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_custom_error_msg_sig.%{env:OXCAML_NAME_MANGLING=flat}.output test_custom_error_msg_sig.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_custom_error_msg_sig.opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_custom_error_msg_sig.mli test_custom_error_msg_sig.ml) filter.sh)
 (action
   (with-outputs-to test_custom_error_msg_sig.opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check all -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_custom_error_msg_sig.opt.%{env:OXCAML_NAME_MANGLING=flat}.output test_custom_error_msg_sig.opt.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_arch_specific_witness.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_arch_specific_witness.ml) filter.sh)
 (action
   (with-outputs-to test_arch_specific_witness.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_arch_specific_witness.%{env:OXCAML_NAME_MANGLING=flat}.output test_arch_specific_witness.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

(rule
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (targets test_sort_witnesses.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)
 (deps (:ml test_sort_witnesses.ml) filter.sh)
 (action
   (with-outputs-to test_sort_witnesses.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected
    (pipe-outputs
    (with-accepted-exit-codes 2
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          -zero-alloc-check default -zero-alloc-checker-details-cutoff 20 -O3))
    (run "./filter.sh")
   ))))

(rule
 (alias   runtest)
 (enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )
 (action (diff test_sort_witnesses.%{env:OXCAML_NAME_MANGLING=flat}.output test_sort_witnesses.%{env:OXCAML_NAME_MANGLING=flat}.output.corrected)))

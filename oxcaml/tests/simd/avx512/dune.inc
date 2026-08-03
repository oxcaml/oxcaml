
(executable
 (name basic512)
 (modules basic512)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_alpha -favx512f -favx512vl -favx512dq -favx512bw ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs512))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (deps avx512_available.exe basic512.exe)
 (action
  (with-outputs-to
   basic512.out
   (run bash -c "if ./avx512_available.exe; then ./basic512.exe; fi"))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected basic512.out)))

(executable
 (name basic512_u)
 (modules basic512_u)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_alpha -favx512f -favx512vl -favx512dq -favx512bw ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs512))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (deps avx512_available.exe basic512_u.exe)
 (action
  (with-outputs-to
   basic512_u.out
   (run bash -c "if ./avx512_available.exe; then ./basic512_u.exe; fi"))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected basic512_u.out)))

(executable
 (name callback512)
 (modules callback512)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_alpha -favx512f -favx512vl -favx512dq -favx512bw ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs512))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (deps avx512_available.exe callback512.exe)
 (action
  (with-outputs-to
   callback512.out
   (run bash -c "if ./avx512_available.exe; then ./callback512.exe; fi"))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected callback512.out)))

(executable
 (name consts512)
 (modules consts512)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_alpha -favx512f -favx512vl -favx512dq -favx512bw ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs512))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (deps avx512_available.exe consts512.exe)
 (action
  (with-outputs-to
   consts512.out
   (run bash -c "if ./avx512_available.exe; then ./consts512.exe; fi"))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected consts512.out)))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (copy basic512.ml basic512_nodynlink.ml)))

(executable
 (name basic512_nodynlink)
 (modules basic512_nodynlink)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_alpha -favx512f -favx512vl -favx512dq -favx512bw -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs512))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (deps avx512_available.exe basic512_nodynlink.exe)
 (action
  (with-outputs-to
   basic512_nodynlink.out
   (run bash -c "if ./avx512_available.exe; then ./basic512_nodynlink.exe; fi"))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected basic512_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (copy basic512_u.ml basic512_u_nodynlink.ml)))

(executable
 (name basic512_u_nodynlink)
 (modules basic512_u_nodynlink)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_alpha -favx512f -favx512vl -favx512dq -favx512bw -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs512))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (deps avx512_available.exe basic512_u_nodynlink.exe)
 (action
  (with-outputs-to
   basic512_u_nodynlink.out
   (run bash -c "if ./avx512_available.exe; then ./basic512_u_nodynlink.exe; fi"))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected basic512_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (copy callback512.ml callback512_nodynlink.ml)))

(executable
 (name callback512_nodynlink)
 (modules callback512_nodynlink)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_alpha -favx512f -favx512vl -favx512dq -favx512bw -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs512))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (deps avx512_available.exe callback512_nodynlink.exe)
 (action
  (with-outputs-to
   callback512_nodynlink.out
   (run bash -c "if ./avx512_available.exe; then ./callback512_nodynlink.exe; fi"))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected callback512_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (copy consts512.ml consts512_nodynlink.ml)))

(executable
 (name consts512_nodynlink)
 (modules consts512_nodynlink)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_alpha -favx512f -favx512vl -favx512dq -favx512bw -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs512))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (deps avx512_available.exe consts512_nodynlink.exe)
 (action
  (with-outputs-to
   consts512_nodynlink.out
   (run bash -c "if ./avx512_available.exe; then ./consts512_nodynlink.exe; fi"))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected consts512_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (copy basic512.ml basic512_internal_assembler.ml)))

(executable
 (name basic512_internal_assembler)
 (modules basic512_internal_assembler)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_alpha -favx512f -favx512vl -favx512dq -favx512bw -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs512))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (deps avx512_available.exe basic512_internal_assembler.exe)
 (action
  (with-outputs-to
   basic512_internal_assembler.out
   (run bash -c "if ./avx512_available.exe; then ./basic512_internal_assembler.exe; fi"))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected basic512_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (copy basic512_u.ml basic512_u_internal_assembler.ml)))

(executable
 (name basic512_u_internal_assembler)
 (modules basic512_u_internal_assembler)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_alpha -favx512f -favx512vl -favx512dq -favx512bw -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs512))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (deps avx512_available.exe basic512_u_internal_assembler.exe)
 (action
  (with-outputs-to
   basic512_u_internal_assembler.out
   (run bash -c "if ./avx512_available.exe; then ./basic512_u_internal_assembler.exe; fi"))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected basic512_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (copy callback512.ml callback512_internal_assembler.ml)))

(executable
 (name callback512_internal_assembler)
 (modules callback512_internal_assembler)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_alpha -favx512f -favx512vl -favx512dq -favx512bw -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs512))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (deps avx512_available.exe callback512_internal_assembler.exe)
 (action
  (with-outputs-to
   callback512_internal_assembler.out
   (run bash -c "if ./avx512_available.exe; then ./callback512_internal_assembler.exe; fi"))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected callback512_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (copy consts512.ml consts512_internal_assembler.ml)))

(executable
 (name consts512_internal_assembler)
 (modules consts512_internal_assembler)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_alpha -favx512f -favx512vl -favx512dq -favx512bw -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs512))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (deps avx512_available.exe consts512_internal_assembler.exe)
 (action
  (with-outputs-to
   consts512_internal_assembler.out
   (run bash -c "if ./avx512_available.exe; then ./consts512_internal_assembler.exe; fi"))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected consts512_internal_assembler.out)))

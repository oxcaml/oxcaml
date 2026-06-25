
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
 (action
  (with-outputs-to
   basic512.out
   (run ./basic512.exe))))

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
 (action
  (with-outputs-to
   basic512_u.out
   (run ./basic512_u.exe))))

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
 (action
  (with-outputs-to
   callback512.out
   (run ./callback512.exe))))

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
 (action
  (with-outputs-to
   consts512.out
   (run ./consts512.exe))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected consts512.out)))

(executable
 (name arrays512)
 (modules arrays512)
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
 (action
  (with-outputs-to
   arrays512.out
   (run ./arrays512.exe))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected arrays512.out)))

(executable
 (name arrays512_u)
 (modules arrays512_u)
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
 (action
  (with-outputs-to
   arrays512_u.out
   (run ./arrays512_u.exe))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected arrays512_u.out)))

(executable
 (name arrays512_vec_mask)
 (modules arrays512_vec_mask)
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
 (action
  (with-outputs-to
   arrays512_vec_mask.out
   (run ./arrays512_vec_mask.exe))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected arrays512_vec_mask.out)))

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
 (action
  (with-outputs-to
   basic512_nodynlink.out
   (run ./basic512_nodynlink.exe))))

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
 (action
  (with-outputs-to
   basic512_u_nodynlink.out
   (run ./basic512_u_nodynlink.exe))))

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
 (action
  (with-outputs-to
   callback512_nodynlink.out
   (run ./callback512_nodynlink.exe))))

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
 (action
  (with-outputs-to
   consts512_nodynlink.out
   (run ./consts512_nodynlink.exe))))

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
  (copy arrays512.ml arrays512_nodynlink.ml)))

(executable
 (name arrays512_nodynlink)
 (modules arrays512_nodynlink)
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
 (action
  (with-outputs-to
   arrays512_nodynlink.out
   (run ./arrays512_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected arrays512_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (copy arrays512_u.ml arrays512_u_nodynlink.ml)))

(executable
 (name arrays512_u_nodynlink)
 (modules arrays512_u_nodynlink)
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
 (action
  (with-outputs-to
   arrays512_u_nodynlink.out
   (run ./arrays512_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected arrays512_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (copy arrays512_vec_mask.ml arrays512_vec_mask_nodynlink.ml)))

(executable
 (name arrays512_vec_mask_nodynlink)
 (modules arrays512_vec_mask_nodynlink)
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
 (action
  (with-outputs-to
   arrays512_vec_mask_nodynlink.out
   (run ./arrays512_vec_mask_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected arrays512_vec_mask_nodynlink.out)))

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
 (action
  (with-outputs-to
   basic512_internal_assembler.out
   (run ./basic512_internal_assembler.exe))))

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
 (action
  (with-outputs-to
   basic512_u_internal_assembler.out
   (run ./basic512_u_internal_assembler.exe))))

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
 (action
  (with-outputs-to
   callback512_internal_assembler.out
   (run ./callback512_internal_assembler.exe))))

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
 (action
  (with-outputs-to
   consts512_internal_assembler.out
   (run ./consts512_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected consts512_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (copy arrays512.ml arrays512_internal_assembler.ml)))

(executable
 (name arrays512_internal_assembler)
 (modules arrays512_internal_assembler)
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
 (action
  (with-outputs-to
   arrays512_internal_assembler.out
   (run ./arrays512_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected arrays512_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (copy arrays512_u.ml arrays512_u_internal_assembler.ml)))

(executable
 (name arrays512_u_internal_assembler)
 (modules arrays512_u_internal_assembler)
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
 (action
  (with-outputs-to
   arrays512_u_internal_assembler.out
   (run ./arrays512_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected arrays512_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (copy arrays512_vec_mask.ml arrays512_vec_mask_internal_assembler.ml)))

(executable
 (name arrays512_vec_mask_internal_assembler)
 (modules arrays512_vec_mask_internal_assembler)
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
 (action
  (with-outputs-to
   arrays512_vec_mask_internal_assembler.out
   (run ./arrays512_vec_mask_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
   (and
    (= %{context_name} "main")
    (= %{architecture} "amd64")
    (<> %{system} macosx)))
 (action
  (diff empty.expected arrays512_vec_mask_internal_assembler.out)))

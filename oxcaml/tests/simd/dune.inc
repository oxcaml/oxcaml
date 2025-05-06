
(executable
 (name basic)
 (modules basic)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   basic.out
   (run ./basic.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected basic.out)))

(executable
 (name basic_u)
 (modules basic_u)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   basic_u.out
   (run ./basic_u.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected basic_u.out)))

(executable
 (name ops_float32x4)
 (modules ops_float32x4)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_float32x4.out
   (run ./ops_float32x4.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_float32x4.out)))

(executable
 (name ops_float32x4_u)
 (modules ops_float32x4_u)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_float32x4_u.out
   (run ./ops_float32x4_u.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_float32x4_u.out)))

(executable
 (name ops_float64x2)
 (modules ops_float64x2)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_float64x2.out
   (run ./ops_float64x2.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_float64x2.out)))

(executable
 (name ops_float64x2_u)
 (modules ops_float64x2_u)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_float64x2_u.out
   (run ./ops_float64x2_u.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_float64x2_u.out)))

(executable
 (name ops_int64x2)
 (modules ops_int64x2)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_int64x2.out
   (run ./ops_int64x2.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_int64x2.out)))

(executable
 (name ops_int64x2_u)
 (modules ops_int64x2_u)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_int64x2_u.out
   (run ./ops_int64x2_u.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_int64x2_u.out)))

(executable
 (name ops_int32x4)
 (modules ops_int32x4)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_int32x4.out
   (run ./ops_int32x4.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_int32x4.out)))

(executable
 (name ops_int32x4_u)
 (modules ops_int32x4_u)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_int32x4_u.out
   (run ./ops_int32x4_u.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_int32x4_u.out)))

(executable
 (name ops_int16x8)
 (modules ops_int16x8)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_int16x8.out
   (run ./ops_int16x8.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_int16x8.out)))

(executable
 (name ops_int16x8_u)
 (modules ops_int16x8_u)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_int16x8_u.out
   (run ./ops_int16x8_u.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_int16x8_u.out)))

(executable
 (name ops_int8x16)
 (modules ops_int8x16)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_int8x16.out
   (run ./ops_int8x16.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_int8x16.out)))

(executable
 (name ops_int8x16_u)
 (modules ops_int8x16_u)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_int8x16_u.out
   (run ./ops_int8x16_u.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_int8x16_u.out)))

(executable
 (name ops)
 (modules ops)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops.out
   (run ./ops.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops.out)))

(executable
 (name ops_u)
 (modules ops_u)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_u.out
   (run ./ops_u.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_u.out)))

(executable
 (name arrays)
 (modules arrays)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   arrays.out
   (run ./arrays.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected arrays.out)))

(executable
 (name arrays_u)
 (modules arrays_u)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   arrays_u.out
   (run ./arrays_u.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected arrays_u.out)))

(executable
 (name scalar_ops)
 (modules scalar_ops)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   scalar_ops.out
   (run ./scalar_ops.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected scalar_ops.out)))

(executable
 (name consts)
 (modules consts)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   consts.out
   (run ./consts.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected consts.out)))

(executable
 (name consts_u)
 (modules consts_u)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   consts_u.out
   (run ./consts_u.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected consts_u.out)))

(executable
 (name callback)
 (modules callback)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   callback.out
   (run ./callback.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected callback.out)))

(executable
 (name test_callee_save_neon_regs)
 (modules test_callee_save_neon_regs)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_callee_save_neon_regs.out
   (run ./test_callee_save_neon_regs.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected test_callee_save_neon_regs.out)))

(executable
 (name probes)
 (modules probes)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard ))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   probes.out
   (run ./probes.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected probes.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy basic.ml basic_nodynlink.ml)))

(executable
 (name basic_nodynlink)
 (modules basic_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   basic_nodynlink.out
   (run ./basic_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected basic_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy basic_u.ml basic_u_nodynlink.ml)))

(executable
 (name basic_u_nodynlink)
 (modules basic_u_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   basic_u_nodynlink.out
   (run ./basic_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected basic_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy ops_float32x4.ml ops_float32x4_nodynlink.ml)))

(executable
 (name ops_float32x4_nodynlink)
 (modules ops_float32x4_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_float32x4_nodynlink.out
   (run ./ops_float32x4_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_float32x4_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy ops_float32x4_u.ml ops_float32x4_u_nodynlink.ml)))

(executable
 (name ops_float32x4_u_nodynlink)
 (modules ops_float32x4_u_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_float32x4_u_nodynlink.out
   (run ./ops_float32x4_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_float32x4_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy ops_float64x2.ml ops_float64x2_nodynlink.ml)))

(executable
 (name ops_float64x2_nodynlink)
 (modules ops_float64x2_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_float64x2_nodynlink.out
   (run ./ops_float64x2_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_float64x2_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy ops_float64x2_u.ml ops_float64x2_u_nodynlink.ml)))

(executable
 (name ops_float64x2_u_nodynlink)
 (modules ops_float64x2_u_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_float64x2_u_nodynlink.out
   (run ./ops_float64x2_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_float64x2_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy ops_int64x2.ml ops_int64x2_nodynlink.ml)))

(executable
 (name ops_int64x2_nodynlink)
 (modules ops_int64x2_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_int64x2_nodynlink.out
   (run ./ops_int64x2_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_int64x2_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy ops_int64x2_u.ml ops_int64x2_u_nodynlink.ml)))

(executable
 (name ops_int64x2_u_nodynlink)
 (modules ops_int64x2_u_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_int64x2_u_nodynlink.out
   (run ./ops_int64x2_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_int64x2_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy ops_int32x4.ml ops_int32x4_nodynlink.ml)))

(executable
 (name ops_int32x4_nodynlink)
 (modules ops_int32x4_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_int32x4_nodynlink.out
   (run ./ops_int32x4_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_int32x4_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy ops_int32x4_u.ml ops_int32x4_u_nodynlink.ml)))

(executable
 (name ops_int32x4_u_nodynlink)
 (modules ops_int32x4_u_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_int32x4_u_nodynlink.out
   (run ./ops_int32x4_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_int32x4_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy ops_int16x8.ml ops_int16x8_nodynlink.ml)))

(executable
 (name ops_int16x8_nodynlink)
 (modules ops_int16x8_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_int16x8_nodynlink.out
   (run ./ops_int16x8_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_int16x8_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy ops_int16x8_u.ml ops_int16x8_u_nodynlink.ml)))

(executable
 (name ops_int16x8_u_nodynlink)
 (modules ops_int16x8_u_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_int16x8_u_nodynlink.out
   (run ./ops_int16x8_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_int16x8_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy ops_int8x16.ml ops_int8x16_nodynlink.ml)))

(executable
 (name ops_int8x16_nodynlink)
 (modules ops_int8x16_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_int8x16_nodynlink.out
   (run ./ops_int8x16_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_int8x16_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy ops_int8x16_u.ml ops_int8x16_u_nodynlink.ml)))

(executable
 (name ops_int8x16_u_nodynlink)
 (modules ops_int8x16_u_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_int8x16_u_nodynlink.out
   (run ./ops_int8x16_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_int8x16_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy ops.ml ops_nodynlink.ml)))

(executable
 (name ops_nodynlink)
 (modules ops_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_nodynlink.out
   (run ./ops_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy ops_u.ml ops_u_nodynlink.ml)))

(executable
 (name ops_u_nodynlink)
 (modules ops_u_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   ops_u_nodynlink.out
   (run ./ops_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected ops_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy arrays.ml arrays_nodynlink.ml)))

(executable
 (name arrays_nodynlink)
 (modules arrays_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   arrays_nodynlink.out
   (run ./arrays_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected arrays_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy arrays_u.ml arrays_u_nodynlink.ml)))

(executable
 (name arrays_u_nodynlink)
 (modules arrays_u_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   arrays_u_nodynlink.out
   (run ./arrays_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected arrays_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy scalar_ops.ml scalar_ops_nodynlink.ml)))

(executable
 (name scalar_ops_nodynlink)
 (modules scalar_ops_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   scalar_ops_nodynlink.out
   (run ./scalar_ops_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected scalar_ops_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy consts.ml consts_nodynlink.ml)))

(executable
 (name consts_nodynlink)
 (modules consts_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   consts_nodynlink.out
   (run ./consts_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected consts_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy consts_u.ml consts_u_nodynlink.ml)))

(executable
 (name consts_u_nodynlink)
 (modules consts_u_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   consts_u_nodynlink.out
   (run ./consts_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected consts_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy callback.ml callback_nodynlink.ml)))

(executable
 (name callback_nodynlink)
 (modules callback_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   callback_nodynlink.out
   (run ./callback_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected callback_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy test_callee_save_neon_regs.ml test_callee_save_neon_regs_nodynlink.ml)))

(executable
 (name test_callee_save_neon_regs_nodynlink)
 (modules test_callee_save_neon_regs_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   test_callee_save_neon_regs_nodynlink.out
   (run ./test_callee_save_neon_regs_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected test_callee_save_neon_regs_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy probes.ml probes_nodynlink.ml)))

(executable
 (name probes_nodynlink)
 (modules probes_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -nodynlink))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   probes_nodynlink.out
   (run ./probes_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected probes_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy basic.ml basic_internal_assembler.ml)))

(executable
 (name basic_internal_assembler)
 (modules basic_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   basic_internal_assembler.out
   (run ./basic_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected basic_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy basic_u.ml basic_u_internal_assembler.ml)))

(executable
 (name basic_u_internal_assembler)
 (modules basic_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   basic_u_internal_assembler.out
   (run ./basic_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected basic_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_float32x4.ml ops_float32x4_internal_assembler.ml)))

(executable
 (name ops_float32x4_internal_assembler)
 (modules ops_float32x4_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_float32x4_internal_assembler.out
   (run ./ops_float32x4_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_float32x4_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_float32x4_u.ml ops_float32x4_u_internal_assembler.ml)))

(executable
 (name ops_float32x4_u_internal_assembler)
 (modules ops_float32x4_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_float32x4_u_internal_assembler.out
   (run ./ops_float32x4_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_float32x4_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_float64x2.ml ops_float64x2_internal_assembler.ml)))

(executable
 (name ops_float64x2_internal_assembler)
 (modules ops_float64x2_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_float64x2_internal_assembler.out
   (run ./ops_float64x2_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_float64x2_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_float64x2_u.ml ops_float64x2_u_internal_assembler.ml)))

(executable
 (name ops_float64x2_u_internal_assembler)
 (modules ops_float64x2_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_float64x2_u_internal_assembler.out
   (run ./ops_float64x2_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_float64x2_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int64x2.ml ops_int64x2_internal_assembler.ml)))

(executable
 (name ops_int64x2_internal_assembler)
 (modules ops_int64x2_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int64x2_internal_assembler.out
   (run ./ops_int64x2_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int64x2_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int64x2_u.ml ops_int64x2_u_internal_assembler.ml)))

(executable
 (name ops_int64x2_u_internal_assembler)
 (modules ops_int64x2_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int64x2_u_internal_assembler.out
   (run ./ops_int64x2_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int64x2_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int32x4.ml ops_int32x4_internal_assembler.ml)))

(executable
 (name ops_int32x4_internal_assembler)
 (modules ops_int32x4_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int32x4_internal_assembler.out
   (run ./ops_int32x4_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int32x4_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int32x4_u.ml ops_int32x4_u_internal_assembler.ml)))

(executable
 (name ops_int32x4_u_internal_assembler)
 (modules ops_int32x4_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int32x4_u_internal_assembler.out
   (run ./ops_int32x4_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int32x4_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int16x8.ml ops_int16x8_internal_assembler.ml)))

(executable
 (name ops_int16x8_internal_assembler)
 (modules ops_int16x8_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int16x8_internal_assembler.out
   (run ./ops_int16x8_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int16x8_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int16x8_u.ml ops_int16x8_u_internal_assembler.ml)))

(executable
 (name ops_int16x8_u_internal_assembler)
 (modules ops_int16x8_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int16x8_u_internal_assembler.out
   (run ./ops_int16x8_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int16x8_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int8x16.ml ops_int8x16_internal_assembler.ml)))

(executable
 (name ops_int8x16_internal_assembler)
 (modules ops_int8x16_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int8x16_internal_assembler.out
   (run ./ops_int8x16_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int8x16_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int8x16_u.ml ops_int8x16_u_internal_assembler.ml)))

(executable
 (name ops_int8x16_u_internal_assembler)
 (modules ops_int8x16_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int8x16_u_internal_assembler.out
   (run ./ops_int8x16_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int8x16_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops.ml ops_internal_assembler.ml)))

(executable
 (name ops_internal_assembler)
 (modules ops_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_internal_assembler.out
   (run ./ops_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_u.ml ops_u_internal_assembler.ml)))

(executable
 (name ops_u_internal_assembler)
 (modules ops_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_u_internal_assembler.out
   (run ./ops_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy arrays.ml arrays_internal_assembler.ml)))

(executable
 (name arrays_internal_assembler)
 (modules arrays_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   arrays_internal_assembler.out
   (run ./arrays_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected arrays_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy arrays_u.ml arrays_u_internal_assembler.ml)))

(executable
 (name arrays_u_internal_assembler)
 (modules arrays_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   arrays_u_internal_assembler.out
   (run ./arrays_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected arrays_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy scalar_ops.ml scalar_ops_internal_assembler.ml)))

(executable
 (name scalar_ops_internal_assembler)
 (modules scalar_ops_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   scalar_ops_internal_assembler.out
   (run ./scalar_ops_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected scalar_ops_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy consts.ml consts_internal_assembler.ml)))

(executable
 (name consts_internal_assembler)
 (modules consts_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   consts_internal_assembler.out
   (run ./consts_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected consts_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy consts_u.ml consts_u_internal_assembler.ml)))

(executable
 (name consts_u_internal_assembler)
 (modules consts_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   consts_u_internal_assembler.out
   (run ./consts_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected consts_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy callback.ml callback_internal_assembler.ml)))

(executable
 (name callback_internal_assembler)
 (modules callback_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   callback_internal_assembler.out
   (run ./callback_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected callback_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy test_callee_save_neon_regs.ml test_callee_save_neon_regs_internal_assembler.ml)))

(executable
 (name test_callee_save_neon_regs_internal_assembler)
 (modules test_callee_save_neon_regs_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   test_callee_save_neon_regs_internal_assembler.out
   (run ./test_callee_save_neon_regs_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected test_callee_save_neon_regs_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy probes.ml probes_internal_assembler.ml)))

(executable
 (name probes_internal_assembler)
 (modules probes_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -internal-assembler))
 (libraries simd_test_helpers simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   probes_internal_assembler.out
   (run ./probes_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected probes_internal_assembler.out)))

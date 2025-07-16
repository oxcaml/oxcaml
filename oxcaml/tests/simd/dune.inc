
(rule
 (targets ops_u.ml)
 (deps ops.ml)
 (action
  (with-stdout-to
   ops_u.ml
   (progn
   (echo "[@@@ocaml.warning \"-unused-module\"]")
   (echo "module Builtins = Builtins_u\n\n")
   (echo "module Utils = Utils_u\n\n")
   (cat "ops.ml")))))

(rule
 (targets ops_float32x4_u.ml)
 (deps ops_float32x4.ml)
 (action
  (with-stdout-to
   ops_float32x4_u.ml
   (progn
   (echo "[@@@ocaml.warning \"-unused-module\"]")
   (echo "module Builtins = Builtins_u\n\n")
   (echo "module Utils = Utils_u\n\n")
   (cat "ops_float32x4.ml")))))

(rule
 (targets ops_float64x2_u.ml)
 (deps ops_float64x2.ml)
 (action
  (with-stdout-to
   ops_float64x2_u.ml
   (progn
   (echo "[@@@ocaml.warning \"-unused-module\"]")
   (echo "module Builtins = Builtins_u\n\n")
   (echo "module Utils = Utils_u\n\n")
   (cat "ops_float64x2.ml")))))

(rule
 (targets ops_int64x2_u.ml)
 (deps ops_int64x2.ml)
 (action
  (with-stdout-to
   ops_int64x2_u.ml
   (progn
   (echo "[@@@ocaml.warning \"-unused-module\"]")
   (echo "module Builtins = Builtins_u\n\n")
   (echo "module Utils = Utils_u\n\n")
   (cat "ops_int64x2.ml")))))

(rule
 (targets ops_int32x4_u.ml)
 (deps ops_int32x4.ml)
 (action
  (with-stdout-to
   ops_int32x4_u.ml
   (progn
   (echo "[@@@ocaml.warning \"-unused-module\"]")
   (echo "module Builtins = Builtins_u\n\n")
   (echo "module Utils = Utils_u\n\n")
   (cat "ops_int32x4.ml")))))

(rule
 (targets ops_int16x8_u.ml)
 (deps ops_int16x8.ml)
 (action
  (with-stdout-to
   ops_int16x8_u.ml
   (progn
   (echo "[@@@ocaml.warning \"-unused-module\"]")
   (echo "module Builtins = Builtins_u\n\n")
   (echo "module Utils = Utils_u\n\n")
   (cat "ops_int16x8.ml")))))

(rule
 (targets ops_int8x16_u.ml)
 (deps ops_int8x16.ml)
 (action
  (with-stdout-to
   ops_int8x16_u.ml
   (progn
   (echo "[@@@ocaml.warning \"-unused-module\"]")
   (echo "module Builtins = Builtins_u\n\n")
   (echo "module Utils = Utils_u\n\n")
   (cat "ops_int8x16.ml")))))

(rule
 (targets ops_float32x8_u.ml)
 (deps ops_float32x8.ml)
 (action
  (with-stdout-to
   ops_float32x8_u.ml
   (progn
   (echo "[@@@ocaml.warning \"-unused-module\"]")
   (echo "module Builtins = Builtins_u\n\n")
   (echo "module Utils = Utils_u\n\n")
   (cat "ops_float32x8.ml")))))

(rule
 (targets ops_float64x4_u.ml)
 (deps ops_float64x4.ml)
 (action
  (with-stdout-to
   ops_float64x4_u.ml
   (progn
   (echo "[@@@ocaml.warning \"-unused-module\"]")
   (echo "module Builtins = Builtins_u\n\n")
   (echo "module Utils = Utils_u\n\n")
   (cat "ops_float64x4.ml")))))

(rule
 (targets ops_int32x8_u.ml)
 (deps ops_int32x8.ml)
 (action
  (with-stdout-to
   ops_int32x8_u.ml
   (progn
   (echo "[@@@ocaml.warning \"-unused-module\"]")
   (echo "module Builtins = Builtins_u\n\n")
   (echo "module Utils = Utils_u\n\n")
   (cat "ops_int32x8.ml")))))

(rule
 (targets ops_int64x4_u.ml)
 (deps ops_int64x4.ml)
 (action
  (with-stdout-to
   ops_int64x4_u.ml
   (progn
   (echo "[@@@ocaml.warning \"-unused-module\"]")
   (echo "module Builtins = Builtins_u\n\n")
   (echo "module Utils = Utils_u\n\n")
   (cat "ops_int64x4.ml")))))

(rule
 (targets ops_int16x16_u.ml)
 (deps ops_int16x16.ml)
 (action
  (with-stdout-to
   ops_int16x16_u.ml
   (progn
   (echo "[@@@ocaml.warning \"-unused-module\"]")
   (echo "module Builtins = Builtins_u\n\n")
   (echo "module Utils = Utils_u\n\n")
   (cat "ops_int16x16.ml")))))

(rule
 (targets ops_int8x32_u.ml)
 (deps ops_int8x32.ml)
 (action
  (with-stdout-to
   ops_int8x32_u.ml
   (progn
   (echo "[@@@ocaml.warning \"-unused-module\"]")
   (echo "module Builtins = Builtins_u\n\n")
   (echo "module Utils = Utils_u\n\n")
   (cat "ops_int8x32.ml")))))

(rule
 (targets sse_other_ops_u.ml)
 (deps sse_other_ops.ml)
 (action
  (with-stdout-to
   sse_other_ops_u.ml
   (progn
   (echo "[@@@ocaml.warning \"-unused-module\"]")
   (echo "module Builtins = Builtins_u\n\n")
   (echo "module Utils = Utils_u\n\n")
   (cat "sse_other_ops.ml")))))

(rule
 (targets sse42_string_ops_u.ml)
 (deps sse42_string_ops.ml)
 (action
  (with-stdout-to
   sse42_string_ops_u.ml
   (progn
   (echo "[@@@ocaml.warning \"-unused-module\"]")
   (echo "module Builtins = Builtins_u\n\n")
   (echo "module Utils = Utils_u\n\n")
   (cat "sse42_string_ops.ml")))))

(executable
 (name basic)
 (modules basic)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
 (name basic256)
 (modules basic256)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   basic256.out
   (run ./basic256.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected basic256.out)))

(executable
 (name basic256_u)
 (modules basic256_u)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   basic256_u.out
   (run ./basic256_u.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected basic256_u.out)))

(executable
 (name let_mutable)
 (modules let_mutable)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   let_mutable.out
   (run ./let_mutable.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected let_mutable.out)))

(executable
 (name let_mutable_u)
 (modules let_mutable_u)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   let_mutable_u.out
   (run ./let_mutable_u.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected let_mutable_u.out)))

(executable
 (name ops_float32x4)
 (modules ops_float32x4)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
 (name sse_other_ops)
 (modules sse_other_ops)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   sse_other_ops.out
   (run ./sse_other_ops.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected sse_other_ops.out)))

(executable
 (name sse_other_ops_u)
 (modules sse_other_ops_u)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   sse_other_ops_u.out
   (run ./sse_other_ops_u.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected sse_other_ops_u.out)))

(executable
 (name sse42_string_ops)
 (modules sse42_string_ops)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   sse42_string_ops.out
   (run ./sse42_string_ops.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected sse42_string_ops.out)))

(executable
 (name sse42_string_ops_u)
 (modules sse42_string_ops_u)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   sse42_string_ops_u.out
   (run ./sse42_string_ops_u.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected sse42_string_ops_u.out)))

(executable
 (name arrays)
 (modules arrays)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
 (name callback256)
 (modules callback256)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   callback256.out
   (run ./callback256.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected callback256.out)))

(executable
 (name test_callee_save_neon_regs)
 (modules test_callee_save_neon_regs)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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

(executable
 (name consts256)
 (modules consts256)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   consts256.out
   (run ./consts256.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected consts256.out)))

(executable
 (name consts256_u)
 (modules consts256_u)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   consts256_u.out
   (run ./consts256_u.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected consts256_u.out)))

(executable
 (name probes256)
 (modules probes256)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   probes256.out
   (run ./probes256.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected probes256.out)))

(executable
 (name utils256)
 (modules utils256)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   utils256.out
   (run ./utils256.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected utils256.out)))

(executable
 (name ops_float32x8)
 (modules ops_float32x8)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_float32x8.out
   (run ./ops_float32x8.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_float32x8.out)))

(executable
 (name ops_float64x4)
 (modules ops_float64x4)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_float64x4.out
   (run ./ops_float64x4.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_float64x4.out)))

(executable
 (name ops_int32x8)
 (modules ops_int32x8)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int32x8.out
   (run ./ops_int32x8.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int32x8.out)))

(executable
 (name ops_int64x4)
 (modules ops_int64x4)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int64x4.out
   (run ./ops_int64x4.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int64x4.out)))

(executable
 (name ops_int16x16)
 (modules ops_int16x16)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int16x16.out
   (run ./ops_int16x16.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int16x16.out)))

(executable
 (name ops_int8x32)
 (modules ops_int8x32)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int8x32.out
   (run ./ops_int8x32.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int8x32.out)))

(executable
 (name ops_float32x8_u)
 (modules ops_float32x8_u)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_float32x8_u.out
   (run ./ops_float32x8_u.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_float32x8_u.out)))

(executable
 (name ops_float64x4_u)
 (modules ops_float64x4_u)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_float64x4_u.out
   (run ./ops_float64x4_u.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_float64x4_u.out)))

(executable
 (name ops_int32x8_u)
 (modules ops_int32x8_u)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int32x8_u.out
   (run ./ops_int32x8_u.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int32x8_u.out)))

(executable
 (name ops_int64x4_u)
 (modules ops_int64x4_u)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int64x4_u.out
   (run ./ops_int64x4_u.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int64x4_u.out)))

(executable
 (name ops_int16x16_u)
 (modules ops_int16x16_u)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int16x16_u.out
   (run ./ops_int16x16_u.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int16x16_u.out)))

(executable
 (name ops_int8x32_u)
 (modules ops_int8x32_u)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta ))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int8x32_u.out
   (run ./ops_int8x32_u.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int8x32_u.out)))

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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy basic256.ml basic256_nodynlink.ml)))

(executable
 (name basic256_nodynlink)
 (modules basic256_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   basic256_nodynlink.out
   (run ./basic256_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected basic256_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy basic256_u.ml basic256_u_nodynlink.ml)))

(executable
 (name basic256_u_nodynlink)
 (modules basic256_u_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   basic256_u_nodynlink.out
   (run ./basic256_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected basic256_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy let_mutable.ml let_mutable_nodynlink.ml)))

(executable
 (name let_mutable_nodynlink)
 (modules let_mutable_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   let_mutable_nodynlink.out
   (run ./let_mutable_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected let_mutable_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (copy let_mutable_u.ml let_mutable_u_nodynlink.ml)))

(executable
 (name let_mutable_u_nodynlink)
 (modules let_mutable_u_nodynlink)
 (enabled_if (= %{context_name} "main"))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if (= %{context_name} "main"))
 (action
  (with-outputs-to
   let_mutable_u_nodynlink.out
   (run ./let_mutable_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (action
   (diff empty.expected let_mutable_u_nodynlink.out)))

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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy sse_other_ops.ml sse_other_ops_nodynlink.ml)))

(executable
 (name sse_other_ops_nodynlink)
 (modules sse_other_ops_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   sse_other_ops_nodynlink.out
   (run ./sse_other_ops_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected sse_other_ops_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy sse_other_ops_u.ml sse_other_ops_u_nodynlink.ml)))

(executable
 (name sse_other_ops_u_nodynlink)
 (modules sse_other_ops_u_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   sse_other_ops_u_nodynlink.out
   (run ./sse_other_ops_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected sse_other_ops_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy sse42_string_ops.ml sse42_string_ops_nodynlink.ml)))

(executable
 (name sse42_string_ops_nodynlink)
 (modules sse42_string_ops_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   sse42_string_ops_nodynlink.out
   (run ./sse42_string_ops_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected sse42_string_ops_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy sse42_string_ops_u.ml sse42_string_ops_u_nodynlink.ml)))

(executable
 (name sse42_string_ops_u_nodynlink)
 (modules sse42_string_ops_u_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   sse42_string_ops_u_nodynlink.out
   (run ./sse42_string_ops_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected sse42_string_ops_u_nodynlink.out)))

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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy callback256.ml callback256_nodynlink.ml)))

(executable
 (name callback256_nodynlink)
 (modules callback256_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   callback256_nodynlink.out
   (run ./callback256_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected callback256_nodynlink.out)))

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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (copy consts256.ml consts256_nodynlink.ml)))

(executable
 (name consts256_nodynlink)
 (modules consts256_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   consts256_nodynlink.out
   (run ./consts256_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected consts256_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy consts256_u.ml consts256_u_nodynlink.ml)))

(executable
 (name consts256_u_nodynlink)
 (modules consts256_u_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   consts256_u_nodynlink.out
   (run ./consts256_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected consts256_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy probes256.ml probes256_nodynlink.ml)))

(executable
 (name probes256_nodynlink)
 (modules probes256_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   probes256_nodynlink.out
   (run ./probes256_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected probes256_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy utils256.ml utils256_nodynlink.ml)))

(executable
 (name utils256_nodynlink)
 (modules utils256_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   utils256_nodynlink.out
   (run ./utils256_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected utils256_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_float32x8.ml ops_float32x8_nodynlink.ml)))

(executable
 (name ops_float32x8_nodynlink)
 (modules ops_float32x8_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_float32x8_nodynlink.out
   (run ./ops_float32x8_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_float32x8_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_float64x4.ml ops_float64x4_nodynlink.ml)))

(executable
 (name ops_float64x4_nodynlink)
 (modules ops_float64x4_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_float64x4_nodynlink.out
   (run ./ops_float64x4_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_float64x4_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int32x8.ml ops_int32x8_nodynlink.ml)))

(executable
 (name ops_int32x8_nodynlink)
 (modules ops_int32x8_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int32x8_nodynlink.out
   (run ./ops_int32x8_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int32x8_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int64x4.ml ops_int64x4_nodynlink.ml)))

(executable
 (name ops_int64x4_nodynlink)
 (modules ops_int64x4_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int64x4_nodynlink.out
   (run ./ops_int64x4_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int64x4_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int16x16.ml ops_int16x16_nodynlink.ml)))

(executable
 (name ops_int16x16_nodynlink)
 (modules ops_int16x16_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int16x16_nodynlink.out
   (run ./ops_int16x16_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int16x16_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int8x32.ml ops_int8x32_nodynlink.ml)))

(executable
 (name ops_int8x32_nodynlink)
 (modules ops_int8x32_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int8x32_nodynlink.out
   (run ./ops_int8x32_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int8x32_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_float32x8_u.ml ops_float32x8_u_nodynlink.ml)))

(executable
 (name ops_float32x8_u_nodynlink)
 (modules ops_float32x8_u_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_float32x8_u_nodynlink.out
   (run ./ops_float32x8_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_float32x8_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_float64x4_u.ml ops_float64x4_u_nodynlink.ml)))

(executable
 (name ops_float64x4_u_nodynlink)
 (modules ops_float64x4_u_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_float64x4_u_nodynlink.out
   (run ./ops_float64x4_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_float64x4_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int32x8_u.ml ops_int32x8_u_nodynlink.ml)))

(executable
 (name ops_int32x8_u_nodynlink)
 (modules ops_int32x8_u_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int32x8_u_nodynlink.out
   (run ./ops_int32x8_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int32x8_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int64x4_u.ml ops_int64x4_u_nodynlink.ml)))

(executable
 (name ops_int64x4_u_nodynlink)
 (modules ops_int64x4_u_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int64x4_u_nodynlink.out
   (run ./ops_int64x4_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int64x4_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int16x16_u.ml ops_int16x16_u_nodynlink.ml)))

(executable
 (name ops_int16x16_u_nodynlink)
 (modules ops_int16x16_u_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int16x16_u_nodynlink.out
   (run ./ops_int16x16_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int16x16_u_nodynlink.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int8x32_u.ml ops_int8x32_u_nodynlink.ml)))

(executable
 (name ops_int8x32_u_nodynlink)
 (modules ops_int8x32_u_nodynlink)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -nodynlink))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int8x32_u_nodynlink.out
   (run ./ops_int8x32_u_nodynlink.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int8x32_u_nodynlink.out)))

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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (copy basic256.ml basic256_internal_assembler.ml)))

(executable
 (name basic256_internal_assembler)
 (modules basic256_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   basic256_internal_assembler.out
   (run ./basic256_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected basic256_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy basic256_u.ml basic256_u_internal_assembler.ml)))

(executable
 (name basic256_u_internal_assembler)
 (modules basic256_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   basic256_u_internal_assembler.out
   (run ./basic256_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected basic256_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy let_mutable.ml let_mutable_internal_assembler.ml)))

(executable
 (name let_mutable_internal_assembler)
 (modules let_mutable_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   let_mutable_internal_assembler.out
   (run ./let_mutable_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected let_mutable_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy let_mutable_u.ml let_mutable_u_internal_assembler.ml)))

(executable
 (name let_mutable_u_internal_assembler)
 (modules let_mutable_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   let_mutable_u_internal_assembler.out
   (run ./let_mutable_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected let_mutable_u_internal_assembler.out)))

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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (copy sse_other_ops.ml sse_other_ops_internal_assembler.ml)))

(executable
 (name sse_other_ops_internal_assembler)
 (modules sse_other_ops_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   sse_other_ops_internal_assembler.out
   (run ./sse_other_ops_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected sse_other_ops_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy sse_other_ops_u.ml sse_other_ops_u_internal_assembler.ml)))

(executable
 (name sse_other_ops_u_internal_assembler)
 (modules sse_other_ops_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   sse_other_ops_u_internal_assembler.out
   (run ./sse_other_ops_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected sse_other_ops_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy sse42_string_ops.ml sse42_string_ops_internal_assembler.ml)))

(executable
 (name sse42_string_ops_internal_assembler)
 (modules sse42_string_ops_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   sse42_string_ops_internal_assembler.out
   (run ./sse42_string_ops_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected sse42_string_ops_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy sse42_string_ops_u.ml sse42_string_ops_u_internal_assembler.ml)))

(executable
 (name sse42_string_ops_u_internal_assembler)
 (modules sse42_string_ops_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   sse42_string_ops_u_internal_assembler.out
   (run ./sse42_string_ops_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected sse42_string_ops_u_internal_assembler.out)))

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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (copy callback256.ml callback256_internal_assembler.ml)))

(executable
 (name callback256_internal_assembler)
 (modules callback256_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   callback256_internal_assembler.out
   (run ./callback256_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected callback256_internal_assembler.out)))

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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
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

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy consts256.ml consts256_internal_assembler.ml)))

(executable
 (name consts256_internal_assembler)
 (modules consts256_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   consts256_internal_assembler.out
   (run ./consts256_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected consts256_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy consts256_u.ml consts256_u_internal_assembler.ml)))

(executable
 (name consts256_u_internal_assembler)
 (modules consts256_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   consts256_u_internal_assembler.out
   (run ./consts256_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected consts256_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy probes256.ml probes256_internal_assembler.ml)))

(executable
 (name probes256_internal_assembler)
 (modules probes256_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   probes256_internal_assembler.out
   (run ./probes256_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected probes256_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy utils256.ml utils256_internal_assembler.ml)))

(executable
 (name utils256_internal_assembler)
 (modules utils256_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   utils256_internal_assembler.out
   (run ./utils256_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected utils256_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_float32x8.ml ops_float32x8_internal_assembler.ml)))

(executable
 (name ops_float32x8_internal_assembler)
 (modules ops_float32x8_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_float32x8_internal_assembler.out
   (run ./ops_float32x8_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_float32x8_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_float64x4.ml ops_float64x4_internal_assembler.ml)))

(executable
 (name ops_float64x4_internal_assembler)
 (modules ops_float64x4_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_float64x4_internal_assembler.out
   (run ./ops_float64x4_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_float64x4_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int32x8.ml ops_int32x8_internal_assembler.ml)))

(executable
 (name ops_int32x8_internal_assembler)
 (modules ops_int32x8_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int32x8_internal_assembler.out
   (run ./ops_int32x8_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int32x8_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int64x4.ml ops_int64x4_internal_assembler.ml)))

(executable
 (name ops_int64x4_internal_assembler)
 (modules ops_int64x4_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int64x4_internal_assembler.out
   (run ./ops_int64x4_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int64x4_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int16x16.ml ops_int16x16_internal_assembler.ml)))

(executable
 (name ops_int16x16_internal_assembler)
 (modules ops_int16x16_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int16x16_internal_assembler.out
   (run ./ops_int16x16_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int16x16_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int8x32.ml ops_int8x32_internal_assembler.ml)))

(executable
 (name ops_int8x32_internal_assembler)
 (modules ops_int8x32_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int8x32_internal_assembler.out
   (run ./ops_int8x32_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int8x32_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_float32x8_u.ml ops_float32x8_u_internal_assembler.ml)))

(executable
 (name ops_float32x8_u_internal_assembler)
 (modules ops_float32x8_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_float32x8_u_internal_assembler.out
   (run ./ops_float32x8_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_float32x8_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_float64x4_u.ml ops_float64x4_u_internal_assembler.ml)))

(executable
 (name ops_float64x4_u_internal_assembler)
 (modules ops_float64x4_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_float64x4_u_internal_assembler.out
   (run ./ops_float64x4_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_float64x4_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int32x8_u.ml ops_int32x8_u_internal_assembler.ml)))

(executable
 (name ops_int32x8_u_internal_assembler)
 (modules ops_int32x8_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int32x8_u_internal_assembler.out
   (run ./ops_int32x8_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int32x8_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int64x4_u.ml ops_int64x4_u_internal_assembler.ml)))

(executable
 (name ops_int64x4_u_internal_assembler)
 (modules ops_int64x4_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int64x4_u_internal_assembler.out
   (run ./ops_int64x4_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int64x4_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int16x16_u.ml ops_int16x16_u_internal_assembler.ml)))

(executable
 (name ops_int16x16_u_internal_assembler)
 (modules ops_int16x16_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int16x16_u_internal_assembler.out
   (run ./ops_int16x16_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int16x16_u_internal_assembler.out)))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (copy ops_int8x32_u.ml ops_int8x32_u_internal_assembler.ml)))

(executable
 (name ops_int8x32_u_internal_assembler)
 (modules ops_int8x32_u_internal_assembler)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (ocamlopt_flags
  (:standard -extension simd_beta -internal-assembler))
 (libraries simd_test_builtins stdlib_stable stdlib_upstream_compatible)
 (foreign_archives stubs))

(rule
 (alias   runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
  (with-outputs-to
   ops_int8x32_u_internal_assembler.out
   (run ./ops_int8x32_u_internal_assembler.exe))))

(rule
 (alias runtest)
 (enabled_if
      (and (= %{context_name} "main")
           (= %{architecture} "amd64")
           (<> %{system} macosx)))
 (action
   (diff empty.expected ops_int8x32_u_internal_assembler.out)))

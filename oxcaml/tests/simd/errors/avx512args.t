Wrong arity should be a diagnostic, not a backend assertion.

  $ cat > arity.ml <<'EOF'
  > external add : (float32x16[@unboxed]) -> (float32x16[@unboxed])
  >   = "" "caml_mm512_add_ps" [@@noalloc] [@@builtin]
  > let f x = add x
  > EOF
  $ ocamlopt.opt -c -extension simd_alpha -favx512f -color never -error-style short arity.ml
  File "arity.ml", line 1:
  Error: Wrong number of arguments for SIMD intrinsic caml_mm512_add_ps
  [2]

An immediate must be constant even when it has the right OCaml type.

  $ cat > nonconst.ml <<'EOF'
  > external cmp : (int[@untagged]) -> (float32x16[@unboxed]) ->
  >   (float32x16[@unboxed]) -> (mask[@unboxed])
  >   = "" "caml_mm512_cmp_ps_mask" [@@noalloc] [@@builtin]
  > let f i x y = cmp i x y
  > EOF
  $ ocamlopt.opt -c -extension simd_alpha -favx512f -color never -error-style short nonconst.ml
  File "nonconst.ml", line 1:
  Error: Did not get integer immediate for caml_mm512_cmp_ps_mask
  [2]

Gather scales must be 1, 2, 4, or 8, not just any integer in range.

  $ cat > scale.ml <<'EOF'
  > external gather : (int[@untagged]) -> (int32x16[@unboxed]) ->
  >   nativeint_u -> (int32x16[@unboxed])
  >   = "" "caml_mm512_i32gather_epi32" [@@noalloc] [@@builtin]
  > let f idx base = gather 3 idx base
  > EOF
  $ ocamlopt.opt -c -extension simd_alpha -favx512f -color never -error-style short scale.ml
  File "scale.ml", line 1:
  Error: Did not get 1, 2, 4, or 8 as scale for caml_mm512_i32gather_epi32
  [2]

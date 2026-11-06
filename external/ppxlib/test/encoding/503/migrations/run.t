This test check that effect patterns are correctly encoded when
migrated down to our 5.2 AST

  $ cat > effect_pattern.ml << EOF
  > let () = try comp () with effect (Xchg n), k -> continue k (n + 1)
  > EOF

  $ ./id_driver.exe effect_pattern.ml
  let () =
    try comp ()
    with
    | [%ppxlib.migration.ppat_effect_503 ? (Xchg n, k)] -> continue k (n + 1)

And that it is correctly decoded when migrated back up to 5.3+ ASTs:

  $ ./id_driver.exe effect_pattern.ml --use-compiler-pp
  let () = try comp () with | effect Xchg n,  k -> continue k (n + 1)

A test for the invalid encoding logic:

  $ cat > effect_pattern_encoded.ml << EOF
  > let () =
  >  try comp ()
  >  with
  >  | [%ppxlib.migration.ppat_effect_503 ? "Badly encoded ppat_effect!"] -> continue k (n + 1)
  > EOF
  $ ./id_driver.exe effect_pattern_encoded.ml --use-compiler-pp 
  File "effect_pattern_encoded.ml", line 4, characters 5-37:
  4 |  | [%ppxlib.migration.ppat_effect_503 ? "Badly encoded ppat_effect!"] -> continue k (n + 1)
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: ppxlib invalid encoding: ppxlib.migration.ppat_effect_503
         
         Ppxlib failed to decode a feature from the OCaml 5.3 AST. This is
         likely caused by the use of a 5.3+ language feature within code
         directly interpreted by a ppx. If you'd like this feature to be
         supported by the ppx involved, please reach out to the ppx
         maintainers. If this does not seem right, please do open an issue at
         https://github.com/ocaml-ppx/ppxlib/issues.
  [1]

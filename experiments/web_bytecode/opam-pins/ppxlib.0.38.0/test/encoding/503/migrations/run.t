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

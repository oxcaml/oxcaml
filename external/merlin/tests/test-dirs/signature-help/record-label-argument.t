Signature help with the cursor in an argument that has a record label.

  $ cat > test.ml << EOF
  > type r = { a : int; mutable b : string }
  > let f (_ : (r, int) idx_imm) (_ : int) = ()
  > let g (_ : int) (_ : int) = ()
  > let _ = f (.a) 3
  > let p = f (.a)
  > let _ = fun (r : r) -> g r.a 3
  > EOF

  $ signature_help () {
  >   $MERLIN single signature-help -position "$1" -filename test.ml < test.ml \
  >     | jq -c '.value | [.signatures[0].label, .activeParameter]'
  > }

A block index argument: on its opening parenthesis, on its label, and just
after its closing parenthesis.

  $ signature_help 4:10
  [null,null]

  $ signature_help 4:12
  [null,null]

  $ signature_help 5:14
  [null,null]

A field access argument: on its label, and just after it.

  $ signature_help 6:27
  [null,null]

  $ signature_help 6:28
  [null,null]

Merlin lexes the quotation syntax by default, so "$" is the splice token
rather than an infix operator.
  $ cat > dollar.ml << EOF
  > let ( $ ) f x = f x
  > let y = succ $ 1
  > EOF

  $ $MERLIN single errors -filename dollar.ml < dollar.ml | revert-newlines | jq '.value[] | .message'
  "Syntax error: let-extension (with punning) expected."

With -no-syntax-quotations, "$" is an ordinary infix operator.
  $ $MERLIN single errors -no-syntax-quotations -filename dollar.ml < dollar.ml | revert-newlines | jq .value
  []

The flag can also come from a .merlin file.
  $ echo "FLG -no-syntax-quotations" > .merlin
  $ $MERLIN single errors -filename dollar.ml < dollar.ml | revert-newlines | jq .value
  []
  $ rm .merlin

-syntax-quotations selects the default explicitly.
  $ $MERLIN single errors -syntax-quotations -filename dollar.ml < dollar.ml | revert-newlines | jq '.value[] | .message'
  "Syntax error: let-extension (with punning) expected."

A #syntax directive in the file takes precedence over the flag.
  $ cat > directive.ml << EOF
  > #syntax quotations off
  > let ( $ ) f x = f x
  > let y = succ $ 1
  > EOF
  $ $MERLIN single errors -syntax-quotations -filename directive.ml < directive.ml | revert-newlines | jq .value
  []

The directive also works the other way round.
  $ cat > directive_on.ml << EOF
  > #syntax quotations on
  > let ( $ ) f x = f x
  > EOF
  $ $MERLIN single errors -no-syntax-quotations -filename directive_on.ml < directive_on.ml | revert-newlines | jq '.value[] | .message'
  "Syntax error: let-extension (with punning) expected."

A stray splice token must produce syntax errors rather than crash parser
recovery.
  $ cat > stray.ml << EOF
  > let f = ( $ )
  > EOF
  $ $MERLIN single errors -filename stray.ml < stray.ml | revert-newlines | jq '.value[] | .message'
  "Splices ($) are not allowed in the initial stage,\nas encountered at file \"stray.ml\", line 1, characters 8-13.\nDid you forget to insert a quotation?"
  "Syntax error after unclosed (, expecting `<['"

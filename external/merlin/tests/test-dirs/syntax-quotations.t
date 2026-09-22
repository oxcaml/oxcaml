Merlin lexes the quotation syntax by default, so "$" is the splice token
rather than an infix operator.
  $ cat > dollar.ml << EOF
  > let ( $ ) f x = f x
  > let y = succ $ 1
  > EOF

  $ $MERLIN single errors -filename dollar.ml < dollar.ml | revert-newlines | jq '.value[] | .type'
  "parser"

With -no-syntax-quotations, "$" is an ordinary infix operator.
  $ $MERLIN single errors -no-syntax-quotations -filename dollar.ml < dollar.ml | revert-newlines | jq .value
  []

The flag can also come from a .merlin file.
  $ echo "FLG -no-syntax-quotations" > .merlin
  $ $MERLIN single errors -filename dollar.ml < dollar.ml | revert-newlines | jq .value
  []
  $ rm .merlin

-syntax-quotations selects the default explicitly.
  $ $MERLIN single errors -syntax-quotations -filename dollar.ml < dollar.ml | revert-newlines | jq '.value[] | .type'
  "parser"

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
  $ $MERLIN single errors -no-syntax-quotations -filename directive_on.ml < directive_on.ml | revert-newlines | jq '.value[] | .type'
  "parser"

A stray splice token must produce syntax errors rather than crash parser
recovery.
  $ cat > stray.ml << EOF
  > let f = ( $ )
  > EOF
  $ $MERLIN single errors -filename stray.ml < stray.ml | revert-newlines | jq '.value[] | .type'
  "typer"
  "parser"

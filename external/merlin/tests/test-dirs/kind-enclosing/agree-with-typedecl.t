This file tests that the first enclosing returned by kind-enclosing agrees with that of
type-enclosing. This is useful because the LSP might want to only display the
kind-enclosing in a hover if it is for the same expression as the type-enclosing hover.

  $ run() {
  >   cat > test.ml
  >   kind=$(
  >     $MERLIN single kind-enclosing -position $1 -filename test.ml < test.ml \
  >     | jq -r '.value[0] | "\(.start.line):\(.start.col)-\(.end.line):\(.end.col)"')
  >   type=$(
  >     $MERLIN single type-enclosing -position $1 -filename test.ml < test.ml \
  >     | jq -r '.value[0] | "\(.start.line):\(.start.col)-\(.end.line):\(.end.col)"')
  >   if [ "$kind" = "$type" ]; then
  >     echo "Agree"
  >   else
  >     echo "Disagree: kind=$kind, type=$type"
  >   fi
  > }

  $ run 1:5 <<EOF
  > type t
  > EOF
  Agree

  $ run 1:9 <<EOF
  > type t = int
  > EOF
  Agree

  $ run 1:9 <<EOF
  > type t = int option
  > EOF
  Agree

  $ run 1:16 <<EOF
  > type t = int option
  > EOF
  Agree

  $ run 1:5 <<EOF
  > let foo bar = bar
  > EOF
  Agree

  $ run 1:9 <<EOF
  > let foo bar = bar
  > EOF
  Agree

  $ run 1:15 <<EOF
  > let foo bar = bar
  > EOF
  Agree

  $ run 2:14 <<EOF
  > let foo =
  >   let x = 1 + 2 + 3 in
  >   x
  > EOF
  Agree

  $ run 2:10 <<EOF
  > let f = function
  > | Some (foo, bar) -> foo
  > EOF
  Agree

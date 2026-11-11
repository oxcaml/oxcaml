The cmi cache is shared by all typer states. A rebuilt .cmi that is re-read on
behalf of one buffer must still invalidate the other buffers that loaded the
old version.

  $ $MERLIN server stop-server
  $ mkdir dep
  $ cat >.merlin <<EOF
  > B dep
  > EOF

  $ build () {
  >   rm -f dep/dep.cmi
  >   printf "$1\n" > dep/dep.mli
  >   $OCAMLC -I dep -c dep/dep.mli
  >   touch -t "200001010000.$(printf %02d $2)" dep/dep.cmi dep
  > }

  $ errors () {
  >   $MERLIN server errors -filename $1 < $1 |
  >     revert-newlines |
  >     jq -r '.value[] | "\(.start.line):\(.start.col) \(.message | gsub("\\s+"; " "))"'
  > }

  $ build 'val x : int' 1
  $ echo 'let _ : string = Dep.x' > a.ml
  $ echo 'let _ : string = Dep.x' > b.ml

  $ errors a.ml
  1:17 The value Dep.x has type int but an expression was expected of type string

Rebuild the dependency, then query b.ml first: its typer state re-reads
dep.cmi into the shared cache.
  $ build 'val x : string' 2
  $ errors b.ml

a.ml must not keep using the old dep.cmi.
  $ errors a.ml

  $ $MERLIN server stop-server

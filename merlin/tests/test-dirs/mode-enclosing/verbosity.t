  $ run() {
  >   cat > test.ml
  >   i=0
  >   while [ "$i" -le 1 ]; do
  >     $MERLIN single mode-enclosing -position "$1" -verbosity "$i" < test.ml \
  >       | jq -r "\"Verbosity $i: \(.value[0].mode)\""
  >     i=$(($i+1))
  >   done
  > }

  $ run 3:11 <<EOF
  > let foo = ref 0
  > let f () =
  >   let _ = foo in
  >   ()
  > EOF
  Verbosity 0: @ portable stateless
  Verbosity 1: @ global portable uncontended read_write stateless aliased many forkable unyielding dynamic

  $ run 2:4 <<EOF
  > let foo = ref 0
  > let f () =
  >   let _ = foo in
  >   ()
  > EOF
  Verbosity 0: @ portable stateless unique static
  Verbosity 1: @ global portable uncontended read_write stateless unique many forkable unyielding static

  $ run() {
  >   cat > test.ml
  >   mode_result=$(
  >     $MERLIN single mode-enclosing -position $1 -filename test.ml < test.ml)
  >   
  >   # Assert that the first enclosing from mode-enclosing is the same as the first
  >   # enclosing from type-enclosing
  >   first_mode_location=$(
  >     echo "$mode_result" \
  >     | jq -r '.value[0] | "\(.start.line):\(.start.col)-\(.end.line):\(.end.col)"')
  >   first_type_location=$(
  >     $MERLIN single type-enclosing -position $1 -filename test.ml < test.ml \
  >     | jq -r '.value[0] | "\(.start.line):\(.start.col)-\(.end.line):\(.end.col)"')
  >   if [ "$first_mode_location" != "$first_type_location" ]; then
  >     echo "Disagrees with type-enclosing: mode=$first_mode_location, type=$first_type_location"
  >   fi
  >   
  >   # Print the result of mode-enclosing
  >   echo "$mode_result" \
  >   | jq -r '.value[] | "\(.start.line):\(.start.col)-\(.end.line):\(.end.col): \(.mode)"'
  > }

Part 1: Test in various syntactic positions

  $ run 1:7 <<EOF
  > let f foo =
  >   foo
  > EOF
  1:6-1:9: @ portable stateless unique static

  $ run 2:4 <<EOF
  > let f foo =
  >   foo
  > EOF
  2:2-2:5: @ portable stateless unique static

  $ run 1:5 <<EOF
  > let foo () = ()
  > EOF
  1:4-1:7: @ portable stateless unique static

  $ run 2:7 <<EOF
  > let f () =
  >   let foo () = () in
  >   ()
  > EOF
  2:6-2:9: @ portable stateless unique static

  $ run 1:5 <<EOF
  > let foo = ()
  > EOF
  1:4-1:7: @ portable stateless unique static

Part 2: Display some interesting modes

  $ run 4:5 <<EOF
  > let counter = ref 0
  > let f_nonportable () = counter := !counter + 1
  > let f () =
  >   f_nonportable ()
  > EOF
  4:2-4:15: @ unique static

In this test, the cursor is on a node causing an error
  $ run 4:5 <<EOF
  > let counter = ref 0
  > let f_nonportable () = counter := !counter + 1
  > let (f @ portable) () =
  >   f_nonportable ()
  > EOF
  4:2-4:15: @ unique static

  $ run 3:12 <<EOF
  > let counter = ref 0
  > let (f @ portable) () =
  >   let _ = counter in
  >   ()
  > EOF
  3:10-3:17: @ portable contended stateless

  $ run 1:5 <<EOF
  > let counter = ref 0
  > let (f @ portable) () =
  >  counter := !counter + 1
  > EOF
  1:4-1:11: @ portable stateless

In this test, bar has all legacy modes
  $ run 4:5 <<EOF
  > let counter = ref 0
  > let foo () =
  >   counter := !counter + (Stdlib.read_int ())
  > let bar = (foo, !counter)
  > EOF
  4:4-4:7: <default>

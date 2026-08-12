  $ locate () {
  >   res=$(cat | $MERLIN single locate -prefix "$1" -position "$2" -context unknown -filename test.ml)
  >   if [ $(echo "$res" | jq .value | jq -r type) = "string" ]; then
  >     echo "$res" | jq -r .value
  >   else
  >     line=$(echo "$res" | jq .value.pos.line)
  >     col=$(echo "$res" | jq .value.pos.col)
  >     echo "$line:$col"
  >   fi
  > }

  $ cat > test.ml <<EOF
  > let x = 0
  > module type S = sig
  >   val y : int
  > end
  > module Foo = struct
  >   module Bar : S = struct
  >     let y = 10
  >   end
  >   let z = 20
  > end
  > type t = A | B
  > type u = { a : int; b : string }
  > let record = { a = 30; b = "hello" }
  > let rec f () =
  >   let w = Foo.Bar.y in
  >   
  > EOF

We can locate identifiers when the cursor is on a new line
  $ locate "x" 16:0 < test.ml
  1:4

  $ locate "S" 16:0 < test.ml
  2:12

  $ locate "Foo" 16:0 < test.ml
  5:7

  $ locate "Foo.Bar" 16:0 < test.ml
  6:9

  $ locate "Foo.Bar.y" 16:0 < test.ml
  3:6

  $ locate "Foo.z" 16:0 < test.ml
  9:6

  $ locate "t" 16:0 < test.ml
  11:5

  $ locate "A" 16:0 < test.ml
  11:9

  $ locate "B" 16:0 < test.ml
  11:13

  $ locate "u" 16:0 < test.ml
  12:5

  $ locate "record" 16:0 < test.ml
  13:4

Record fields are weird
  $ locate "record.a" 16:0 < test.ml
  didn't manage to find record.a

  $ locate "record.b" 16:0 < test.ml
  didn't manage to find record.b

  $ locate "a" 16:0 < test.ml
  12:11

  $ locate "b" 16:0 < test.ml
  12:20

  $ locate "w" 16:0 < test.ml
  15:6

  $ locate "f" 16:0 < test.ml
  14:8

We can locate identifiers when the cursor is within a module path
  $ locate "x" 15:15 < test.ml
  1:4

  $ locate "S" 15:15 < test.ml
  2:12

  $ locate "Foo" 15:15 < test.ml
  5:7

  $ locate "Foo.Bar" 15:15 < test.ml
  6:9

  $ locate "Foo.Bar.y" 15:15 < test.ml
  3:6

  $ locate "Foo.z" 15:15 < test.ml
  9:6

  $ locate "t" 15:15 < test.ml
  11:5

  $ locate "A" 15:15 < test.ml
  11:9

  $ locate "B" 15:15 < test.ml
  11:13

  $ locate "u" 15:15 < test.ml
  12:5

  $ locate "record" 15:15 < test.ml
  13:4

  $ locate "record.a" 15:15 < test.ml
  didn't manage to find record.a

  $ locate "record.b" 15:15 < test.ml
  didn't manage to find record.b

  $ locate "a" 15:15 < test.ml
  12:11

  $ locate "b" 15:15 < test.ml
  12:20

  $ locate "w" 15:15 < test.ml
  Not in environment 'w'

  $ locate "f" 15:15 < test.ml
  14:8

Prioritizes values, then types, then module signatures.
(Other kinds of namespaces cannot contain identifiers starting with a lowercase)

  $ locate "x" 5:0 <<EOF
  > let x = 10
  > type x
  > module type x = sig end
  > let () =
  > 
  > EOF
  1:4

  $ locate "x" 5:0 <<EOF
  > (* let x = 10 *)
  > type x
  > module type x = sig end
  > let () =
  > 
  > EOF
  2:5

  $ locate "x" 5:0 <<EOF
  > (* let x = 10 *)
  > (* type x *)
  > module type x = sig end
  > let () =
  > 
  > EOF
  3:12

Prioritizes constructors, then modules, then module signatures.
(Other kinds of namespaces cannot contain identifiers starting with an uppercase)

  $ locate "X" 5:0 <<EOF
  > type _ = X
  > module X = struct end
  > module type X = sig end
  > let () =
  > 
  > EOF
  1:9

  $ locate "X" 5:0 <<EOF
  > (* type _ = X *)
  > module X = struct end
  > module type X = sig end
  > let () =
  > 
  > EOF
  2:7

  $ locate "X" 5:0 <<EOF
  > (* type _ = X *)
  > (* module X = struct end *)
  > module type X = sig end
  > let () =
  > 
  > EOF
  3:12

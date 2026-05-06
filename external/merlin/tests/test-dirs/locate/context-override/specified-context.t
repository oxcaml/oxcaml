  $ cat > test.ml <<EOF
  > module type Foo = sig
  >   type foo
  >   val foo : int
  >   module type Foo = sig end
  >   module Foo : Foo
  > end
  > module Foo : Foo = struct
  >   type foo = | Foo
  >   let foo = 20
  >   module type Foo = sig end
  >   module Foo = struct end
  > end
  > type foo = | Foo
  > type bar = { foo : foo }
  > let foo = { foo = _ }
  > let () =
  >   
  > EOF

  $ locate () {
  >   res=$(cat | $MERLIN single locate -prefix "$2" -position "$3" -context "$1" -filename test.ml < test.ml)
  >   if [ $(echo "$res" | jq .value | jq -r type) = "string" ]; then
  >     echo "$res" | jq -r .value
  >   else
  >     line=$(echo "$res" | jq .value.pos.line)
  >     col=$(echo "$res" | jq .value.pos.col)
  >     echo "$line:$col"
  >   fi
  > }

Iterate over each interesting identifier in each possible context and print results
  $ for context in expr module_path module_type pattern type constant constructor label unknown
  > do
  >   echo "Context: $context"
  >   for identifier in foo Foo Foo.foo Foo.Foo
  >   do
  >     res=$(cat | $MERLIN single locate -prefix "$identifier" -position 17:0 -context "$context" -filename test.ml < test.ml)
  >     if [ $(echo "$res" | jq .value | jq -r type) = "string" ]; then
  >       output=$(echo "$res" | jq -r .value)
  >     else
  >       line_num=$(echo "$res" | jq .value.pos.line)
  >       col=$(echo "$res" | jq .value.pos.col)
  >       line=$(sed -n "${line_num}p" test.ml | xargs)
  >       output="$line ($line_num:$col)"
  >     fi
  >     echo "  $identifier: $output"
  >   done
  >   echo
  > done
  Context: expr
    foo: let foo = { foo = _ } (15:4)
    Foo: module Foo : Foo = struct (7:7)
    Foo.foo: val foo : int (3:6)
    Foo.Foo: module Foo : Foo (5:9)
  
  Context: module_path
    foo: Not in environment 'foo'
    Foo: module Foo : Foo = struct (7:7)
    Foo.foo: Not in environment 'Foo.foo'
    Foo.Foo: module Foo : Foo (5:9)
  
  Context: module_type
    foo: type foo = | Foo (13:5)
    Foo: module type Foo = sig (1:12)
    Foo.foo: type foo (2:7)
    Foo.Foo: module type Foo = sig end (4:14)
  
  Context: pattern
    foo: type foo = | Foo (13:5)
    Foo: module Foo : Foo = struct (7:7)
    Foo.foo: type foo (2:7)
    Foo.Foo: module Foo : Foo (5:9)
  
  Context: type
    foo: type foo = | Foo (13:5)
    Foo: module Foo : Foo = struct (7:7)
    Foo.foo: type foo (2:7)
    Foo.Foo: module Foo : Foo (5:9)
  
  Context: constant
    foo: let foo = { foo = _ } (15:4)
    Foo: module Foo : Foo = struct (7:7)
    Foo.foo: val foo : int (3:6)
    Foo.Foo: module Foo : Foo (5:9)
  
  Context: constructor
    foo: let foo = { foo = _ } (15:4)
    Foo: type foo = | Foo (13:13)
    Foo.foo: val foo : int (3:6)
    Foo.Foo: module Foo : Foo (5:9)
  
  Context: label
    foo: type bar = { foo : foo } (14:13)
    Foo: module Foo : Foo = struct (7:7)
    Foo.foo: val foo : int (3:6)
    Foo.Foo: module Foo : Foo (5:9)
  
  Context: unknown
    foo: let foo = { foo = _ } (15:4)
    Foo: type foo = | Foo (13:13)
    Foo.foo: val foo : int (3:6)
    Foo.Foo: module Foo : Foo (5:9)
  

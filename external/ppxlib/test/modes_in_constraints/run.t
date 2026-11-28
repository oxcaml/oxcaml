Modes in type constraints go through the driver, and the compiler still checks them.

  $ check() { ocamlc -stop-after typing -ppx './driver.exe -as-ppx' -c test.ml; }

Check that modes in type constraints parse and are not dropped.

  $ cat > test.ml <<EOF
  > let () = (() : _ @ unique)
  > EOF
  $ check

  $ cat > test.ml <<EOF
  > let _ = ((fun x -> x) : 'a -> 'a @ local unique)
  > EOF
  $ check
  File "test.ml", line 1, characters 19-20:
  1 | let _ = ((fun x -> x) : 'a -> 'a @ local unique)
                         ^
  Error: This value is aliased but is expected to be unique.
  [2]

Check that modes aren't dropped for let-bindings with type annotations. The bindings
which must be global are named, as the compiler doesn't require [let _] to be.

  $ cat > test.ml <<EOF
  > let () =
  >   let f : _ @ once = fun () -> () in
  >   f ();
  >   f ()
  > EOF
  $ check
  File "test.ml", line 4, characters 2-3:
  4 |   f ()
        ^
  Error: This value is used here,
         but it is defined as once and has already been used at:
  File "test.ml", line 3, characters 2-3:
  3 |   f ();
        ^
    
  [2]

  $ cat > test.ml <<EOF
  > let x : string =
  >   let f : ('a -> local_ 'a) = fun x -> x in
  >   f "I shouldn't escape"
  > EOF
  $ check
  File "test.ml", line 1, characters 4-5:
  1 | let x : string =
          ^
  Error: The expression is local
         but is expected to be global
           because it is the value x in the structure at file "test.ml", lines 1-3, characters 0-24
           which is expected to be global
           because modules always need to be allocated on the heap.
  [2]

  $ cat > test.ml <<EOF
  > let x : string =
  >   let f (local_ x) = x in
  >   f "I shouldn't escape"
  > EOF
  $ check
  File "test.ml", line 1, characters 4-5:
  1 | let x : string =
          ^
  Error: The expression is local
         but is expected to be global
           because it is the value x in the structure at file "test.ml", lines 1-3, characters 0-24
           which is expected to be global
           because modules always need to be allocated on the heap.
  [2]

  $ cat > test.ml <<EOF
  > let x : string =
  >   let f (x @ local) = x in
  >   f "I shouldn't escape"
  > EOF
  $ check
  File "test.ml", line 1, characters 4-5:
  1 | let x : string =
          ^
  Error: The expression is local
         but is expected to be global
           because it is the value x in the structure at file "test.ml", lines 1-3, characters 0-24
           which is expected to be global
           because modules always need to be allocated on the heap.
  [2]

  $ cat > test.ml <<EOF
  > let g =
  >   let (f @ local) x = x in
  >   f
  > EOF
  $ check
  File "test.ml", line 1, characters 4-5:
  1 | let g =
          ^
  Error: The expression is local
         but is expected to be global
           because it is the value g in the structure at file "test.ml", lines 1-3, characters 0-3
           which is expected to be global
           because modules always need to be allocated on the heap.
  [2]

  $ cat > test.ml <<EOF
  > let _ : int ref @ unique = ref 1
  > EOF
  $ check
  File "test.ml", line 1, characters 27-32:
  1 | let _ : int ref @ unique = ref 1
                                 ^^^^^
  Error: This value is aliased but is expected to be unique.
  [2]

Because it can't hurt, some non-erroring examples.

  $ cat > test.ml <<EOF
  > let _ : _ =
  >   let f : ('a -> local_ 'a) = fun x -> x in
  >   f
  > EOF
  $ check

  $ cat > test.ml <<EOF
  > let _ : int =
  >   let f : ('a -> local_ 'a) = fun x -> x in
  >   f 5
  > EOF
  $ check

  $ cat > test.ml <<EOF
  > let _ : int =
  >   let f (local_ x) = x in
  >   f 5
  > EOF
  $ check

  $ cat > test.ml <<EOF
  > let _ : int =
  >   let f (x @ local) = x in
  >   f 5
  > EOF
  $ check

  $ cat > test.ml <<EOF
  > let _ : int @ unique = 1
  > EOF
  $ check

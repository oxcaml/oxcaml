Test recovery from mode-related errors

Error from an escape lock
  $ cat > escape.ml <<EOF
  > let () =
  >   let x = local_ "" in
  >   let module M = struct
  >     class foo = object
  >       val y = x
  >     end
  >   end in
  >   ()
  > EOF

The error is reported
  $ $MERLIN single errors -filename escape.ml < escape.ml | revert-newlines \
  >   | jq .value[].message -r
  The value x is local but is expected to be global
  because it is used in a class (at File "escape.ml", line 4, characters 16-46).

We can locate the value that was used incorrectly
  $ $MERLIN single locate -position 5:15 -filename escape.ml < escape.ml | jq .value.pos -c
  {"line":2,"col":6}

Error from a share lock
  $ cat > share.ml <<EOF
  > let () =
  >   let f : (unit -> unit) @ once = fun () -> () in
  >   for _ = 1 to 10 do
  >     let g = f in
  >     for _ = 1 to 10 do
  >       g ()
  >     done
  >   done
  > EOF

The error is reported and f still has mode once
  $ $MERLIN single errors -filename share.ml < share.ml | revert-newlines \
  >   | jq .value[].message -r
  The value f is once but is expected to be many
  because it is used in a loop (at File "share.ml", line 3, characters 2-87).
  The value g is once but is expected to be many
  because it is used in a loop (at File "share.ml", line 5, characters 4-42).

We can locate the values that were used incorrectly
  $ $MERLIN single locate -position 4:12 -filename share.ml < share.ml | jq .value.pos -c
  {"line":2,"col":6}
  $ $MERLIN single locate -position 6:6 -filename share.ml < share.ml | jq .value.pos -c
  {"line":4,"col":8}

Error from a closure lock
  $ cat > closure1.ml <<EOF
  > module List : sig
  >   val iter : f:('a -> unit) -> 'a list -> unit
  > end = struct
  >   let iter = failwith ""
  > end
  > let length xs =
  >   let local_ count = ref 0 in
  >   List.iter xs ~f:(fun () -> incr count);
  >   !count
  > EOF

The error is reported
  $ $MERLIN single errors -filename closure1.ml < closure1.ml \
  >   | revert-newlines \
  >   | jq .value[].message -r
  The value count is local but is expected to be global
  because it is used inside the function at File "closure1.ml", line 8, characters 18-40
  which is expected to be global.

We can locate the value that was used incorrectly
  $ $MERLIN single locate -position 8:37 -filename closure1.ml < closure1.ml | jq .value.pos -c
  {"line":7,"col":13}

Error from closure lock
  $ cat > closure2.ml <<EOF
  > let y = ref 5
  > let foo x =
  >   y := !y + 1;
  >   x
  > ;;
  > let (f @ portable) x =
  >   let bar = foo in
  >   let (g @ portable) y =
  >     bar y
  >   in
  >   g x
  > EOF

The error is reported and foo still has mode nonportable
  $ $MERLIN single errors -filename closure2.ml < closure2.ml \
  >   | revert-newlines \
  >   | jq .value[].message -r
  The value foo is nonportable
  because it contains a usage (of the value y at File "closure2.ml", line 3, characters 2-3)
  which is expected to be uncontended.
  However, the value foo highlighted is expected to be portable
  because it is used inside the function at File "closure2.ml", line 6, characters 19-87
  which is expected to be portable.
  The value bar is nonportable but is expected to be portable
  because it is used inside the function at File "closure2.ml", line 8, characters 21-34
  which is expected to be portable.

We can locate the values that were used incorrectly
  $ $MERLIN single locate -position 7:13 -filename closrue2.ml < closure2.ml | jq .value.pos -c
  {"line":2,"col":4}
  $ $MERLIN single locate -position 9:8 -filename closrue2.ml < closure2.ml | jq .value.pos -c
  {"line":8,"col":21}

Error from exclave lock
  $ cat > exclave.ml <<EOF
  > let f () =
  >   let x = local_ Some 10 in
  >   exclave_ x
  > EOF

The error is reported
  $ $MERLIN single errors -filename exclave.ml < exclave.ml | jq .value[].message -r
  The value x is local, so it cannot be used inside an exclave_

We can locate the value that was used incorrectly
  $ $MERLIN single locate -position 3:11 -filename exclave.ml < exclave.ml | jq .value.pos -c
  {"line":2,"col":6}

Error from unboxed lock
  $ cat > unboxed.ml <<EOF
  > let f : unit -> ('a : bits32) = assert false
  > let _ =
  >   let x = f () in
  >   object
  >     method bar = x
  >   end
  > EOF

The error is reported
  $ $MERLIN single errors -filename unboxed.ml < unboxed.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 17
        },
        "end": {
          "line": 5,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "x must have a type of layout value because it is captured by an object.
  The layout of 'a is bits32
    because of the definition of x at file \"unboxed.ml\", line 3, characters 10-14.
  But the layout of 'a must overlap with value
    because it's the type of a variable captured in an object."
      }
    ],
    "notifications": []
  }

We can locate the value that was used incorrectly
  $ $MERLIN single locate -position 5:18 -filename unboxed.ml < unboxed.ml | jq .value.pos -c
  {"line":3,"col":6}

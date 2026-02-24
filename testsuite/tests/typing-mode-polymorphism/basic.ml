(* TEST
 flags += "-extension mode_polymorphism_alpha";
 expect;
*)

(*
 * This file tests that mode polymorphism works, without printing mode variables.
 * The modes printed are not always representative of the underlying modes: they have
 * been zapped in order to be printed
*)

let use_uncontended (x @ uncontended) = ()
let use_portable (x @ portable) = ()
let use_unique (x @ unique) = ()
let use_static (x @ static) = ()
let use_global (x @ global) = ()
[%%expect{|
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val use_unique : 'a @ unique -> unit = <fun>
val use_static : 'a -> unit = <fun>
val use_global : 'a -> unit = <fun>
|}]

(* BASIC POLYMORPHISM *)

let foo =
  let foo x = x in
  let x = ref 42 in
  let x = foo x in
  let (y @ contended) = ref !x in
  let _ = foo y in
  foo
[%%expect{|
Line 6, characters 14-15:
6 |   let _ = foo y in
                  ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let id x = x
[%%expect{|
val id : 'a -> 'a = <fun>
|}]

let () =
  let x = ref 42 in
  let x = id x in
  let (y @ contended) = ref !x in
  let _ = id y in
  ()
[%%expect{|
Line 5, characters 13-14:
5 |   let _ = id y in
                 ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* instantiation [id] does not make it less polymorphic *)
let foo (x @ portable) = id x
let id' = id
[%%expect{|
val foo : 'a @ portable -> 'a = <fun>
val id' : 'a -> 'a = <fun>
|}]

let foo (x @ nonportable) =
  let x = id' x in
  use_portable x
[%%expect{|
Line 3, characters 15-16:
3 |   use_portable x
                   ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

let bar (c @ unique) =
  use_unique (id c)
[%%expect{|
Line 2, characters 13-19:
2 |   use_unique (id c)
                 ^^^^^^
Error: This value is "aliased" but is expected to be "unique".
|}]

let bar (c @ local) =
  let _ = use_unique (id c) in
  ()
[%%expect{|
Line 2, characters 25-26:
2 |   let _ = use_unique (id c) in
                             ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let bar (x @ aliased) =
  let x = id x in
  let _ = use_unique x in
  ()
[%%expect{|
Line 3, characters 21-22:
3 |   let _ = use_unique x in
                         ^
Error: This value is "aliased" but is expected to be "unique".
|}]

let bar (x @ many) =
  let y @ once = x in
  let y = id y in
  (y, y)
[%%expect{|
Line 3, characters 13-14:
3 |   let y = id y in
                 ^
Error: This value is "once" but is expected to be "many".
|}]

(* the result of a mode polymorphic function can't be static *)
let foo (x @ static) =
  let x = id x in
  use_static x
[%%expect{|
Line 3, characters 13-14:
3 |   use_static x
                 ^
Error: This value is "dynamic" but is expected to be "static".
|}]

(* mode polymorphism allows us to combine take combine the bounds of two
functions via joins *)
let f (x : string) = x
let g (x : string @ local) = x
let which = function
  | false -> f
  | true -> g
[%%expect{|
val f : string -> string = <fun>
val g : string @ local -> string @ local = <fun>
Line 5, characters 12-13:
5 |   | true -> g
                ^
Error: This expression has type "string @ local -> string @ local"
       but an expression was expected of type "string -> string"
|}]

(* The least upper bound between local and global is local *)
let foo (x @ global) =
  let f = which true in
  let y @ global = "global" in
  use_global (f y) (* y is weakened to local before it's applied to f *)
[%%expect{|
Line 2, characters 10-15:
2 |   let f = which true in
              ^^^^^
Error: Unbound value "which"
|}]

let foo (x @ global) =
  let f = which false in
  let y @ global = "global" in
  use_global (f y) (* y is weakened to local before it's applied to f *)
[%%expect{|
Line 2, characters 10-15:
2 |   let f = which false in
              ^^^^^
Error: Unbound value "which"
|}]

(* mode variables used at some mode imposes a bound on them *)
let id x = use_portable x; x
[%%expect{|
val id : 'a @ portable -> 'a = <fun>
|}]

let foo (x @ nonportable) =
  id x
[%%expect{|
Line 2, characters 5-6:
2 |   id x
         ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* but they remain polymorphic on the other axes *)

let foo (x @ contended) (y @ uncontended) =
  let x = id x in
  let y = id y in
  use_uncontended y; (* this use succeeds *)
  use_uncontended x (* this use fails *)
[%%expect{|
Line 2, characters 13-14:
2 |   let x = id x in
                 ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* CLOSING OVER MODE VARIABLES *)
(* Basic closing-over behavior. See [currying.ml] for more intricate patterns *)

let close_over x = fun () -> x
[%%expect{|
val close_over : 'a -> unit -> 'a = <fun>
|}]

let foo (x @ portable) (y @ nonportable) =
  let const_x = close_over x in
  let const_y = close_over y in
  use_portable (const_x ());
  use_portable (const_y ())
[%%expect{|
Line 4, characters 15-27:
4 |   use_portable (const_x ());
                   ^^^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

let close_over x = fun () -> fun () -> x
[%%expect{|
val close_over : 'a -> unit -> unit -> 'a = <fun>
|}]

let foo (x @ portable) (y @ nonportable) =
  let const_x = close_over x in
  let const_y = close_over y in
  use_portable (const_x () ());
  use_portable (const_y () ())
[%%expect{|
Line 4, characters 15-30:
4 |   use_portable (const_x () ());
                   ^^^^^^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* partially applying a nested closure similarly depends on input mode *)
let foo (x @ portable) =
  let const_x = close_over x in
  use_portable (const_x ())
[%%expect{|
Line 3, characters 15-27:
3 |   use_portable (const_x ())
                   ^^^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

let foo (x @ portable) =
  use_portable (close_over x)
[%%expect{|
Line 2, characters 15-29:
2 |   use_portable (close_over x)
                   ^^^^^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* MODE CROSSING *)

(* mode crossing is stronger than mode polymorphism *)
let foo (x : int @ portable) (y : int @ nonportable) =
  let x = id x in
  let y = id y in
  use_portable x;
  use_portable y
[%%expect{|
val foo : int @ portable -> int -> unit = <fun>
|}]

(* LOCAL AND MODE POLYMORPHISM *)

(* local values stay local through id - can't return without exclave_ *)
let foo (local_ x) =
  let y = id x in
  y
[%%expect{|
Line 2, characters 13-14:
2 |   let y = id x in
                 ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

(* with exclave_ it works *)
let foo (local_ x) = exclave_
  let y = id x in
  y
[%%expect{|
Line 2, characters 13-14:
2 |   let y = id x in
                 ^
Error: This value is "local" but is expected to be "global".
|}]

(* local input stays local through id *)
let foo () =
  let x @ local = "hello" in
  let y = id x in
  use_global y
[%%expect{|
Line 3, characters 13-14:
3 |   let y = id x in
                 ^
Error: This value is "local" but is expected to be "global".
|}]

(* MULTIPLE MODE AXES *)

(* Bounds can be imposed on multiple axes *)
let foo (x @ global portable) =
  use_global (id x);
  use_portable (id x)
[%%expect{|
Line 3, characters 15-21:
3 |   use_portable (id x)
                   ^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

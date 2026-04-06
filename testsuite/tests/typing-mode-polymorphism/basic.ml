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
val foo : '_weak1 -> '_weak1 = <fun>
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
val bar : 'a @ unique -> unit = <fun>
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
Line 4, characters 6-7:
4 |   (y, y)
          ^
Error: This value is used here,
       but it is defined as once and is also being used at:
Line 4, characters 3-4:
4 |   (y, y)
       ^

|}]

(* the result of a mode polymorphic function can't be static *)
let foo (x @ static) =
  let x = id x in
  use_static x
[%%expect{|
Line 3, characters 13-14:
3 |   use_static x
                 ^
Error: This value is "dynamic"
         because function applications are always dynamic.
       However, the highlighted expression is expected to be "static".
|}]

(* mode polymorphism allows us to combine take combine the bounds of two
functions via joins *)
let f (x : string) = x
let g (x : string @ portable) = x
let which = function
  | false -> f
  | true -> g
[%%expect{|
val f : string -> string = <fun>
val g : string @ portable -> string = <fun>
val which : bool -> string @ portable -> string = <fun>
|}]

(* The least upper bound between portable and nonportable is nonportable *)
let foo (x @ portable) =
  let f = which true in
  use_portable (f x) (* x is weakened to nonportable before it's applied to f *)
[%%expect{|
val foo : string @ portable -> unit = <fun>
|}]

let foo (x @ portable) =
  let f = which false in
  use_global (f x) (* x is weakened to nonportable before it's applied to f *)
[%%expect{|
val foo : string @ portable -> unit = <fun>
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
Line 5, characters 18-19:
5 |   use_uncontended x (* this use fails *)
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
Line 5, characters 15-27:
5 |   use_portable (const_y ())
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
Line 5, characters 15-30:
5 |   use_portable (const_y () ())
                   ^^^^^^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* partially applying a nested closure similarly depends on input mode *)
let foo (x @ portable) =
  let const_x = close_over x in
  use_portable (const_x ())
[%%expect{|
val foo : 'a @ portable -> unit = <fun>
|}]

let foo (x @ portable) =
  use_portable (close_over x)
[%%expect{|
val foo : 'a @ portable -> unit = <fun>
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

(* When translating a function, the compiler must decide where to allocate the return
  value. If we allow the return value to be polymorphic, we must either forego all
  optimisations and default to heap allocations in all cases, or we limit polymorphism
  over return values.

  If we don't, we might encounter a soundness issue, where a return value is used as
  [global] but the compiler chose to allocate it on the stack.

  The following tests show the limits of polymorphism over locality
*)

(* identity forces the return value to be global *)
let id x = x

(* but we can still annotate the function to (always) return a local value *)
let id_local (x @ local) = x
let id_exclave x = exclave_ x
[%%expect{|
val id : 'a -> 'a = <fun>
val id_local : 'a @ local -> 'a @ local = <fun>
val id_exclave : 'a -> 'a @ local = <fun>
|}]

(* Functions impose a default global bound over return values. As a result, [id]
  only accepts global values  *)
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

(* When defined locally, [id] can be made to default to local values instead *)
let foo () =
  let id x = x in
  let x @ local = "local" in
  let y @ global = "global" in
  let _ = id x in
  let z = id y in
  use_global z (* ought to fail since id defaults to local and weakens to y *)
[%%expect{|
Line 7, characters 13-14:
7 |   use_global z (* ought to fail since id defaults to local and weakens to y *)
                 ^
Error: This value is "local" but is expected to be "global".
|}]

(* if return values are only used as global it allocates on the heap *)
let foo () =
  let id x = x in
  let y @ global = "global" in
  let z = id y in
  use_global z (* succeeds *)
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* local values stay local through id_local - can't return without exclave_ *)
let foo (local_ x) =
  let y = id_local x in
  y
[%%expect{|
Line 3, characters 2-3:
3 |   y
      ^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

(* with exclave_ it works *)
let foo (local_ x) = exclave_
  let y = id_local x in
  y
[%%expect{|
val foo : 'a @ local -> 'a @ local = <fun>
|}]

(* MULTIPLE MODE AXES *)

(* Bounds can be imposed on multiple axes *)
let foo (x @ uncontended portable) =
  use_uncontended (id x);
  use_portable (id x)
[%%expect{|
val foo : 'a @ portable -> unit = <fun>
|}]

(* TEST
 flags += "-extension mode_polymorphism_alpha";
 expect;
*)

let use_uncontended (x @ uncontended) = ()
let use_portable (x @ portable) = ()
let use_unique (x @ unique) = ()
let use_static (x @ static) = ()
let use_global (x @ global) = ()
[%%expect{|
val use_uncontended : 'a @ [< uncontended] -> unit @ 'm = <fun>
val use_portable : 'a @ [< portable] -> unit @ 'm = <fun>
val use_unique : 'a @ [< unique] -> unit @ 'm = <fun>
val use_static : 'a @ 'n -> unit @ 'm = <fun>
val use_global : 'a @ [< global] -> unit @ 'm = <fun>
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
val foo : '_weak1 @ unique stateless -> '_weak1 @ 'm = <fun>
|}]

let id x = x
[%%expect{|
val id : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
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
val foo : 'a @ [< 'm & global portable] -> 'a @ [> 'm] = <fun>
val id' : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
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
val bar : 'a @ [< global unique] -> unit @ 'm = <fun>
|}]

let bar (c @ local) =
  let _ = use_unique (id c) in
  ()
[%%expect{|
val bar : 'a @ [< unique > local] -> unit @ 'm = <fun>
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
let g (x : string @ local) = x
let which = function
  | false -> f
  | true -> g
[%%expect{|
val f : string @ [< 'm . contended] -> string @ [> 'm @@ many portable] =
  <fun>
val g :
  string @ [< 'm . contended > local] ->
  string @ [> 'm @@ many portable | local] = <fun>
val which :
  bool @ 'n ->
  (string @ [< 'm . contended > local] ->
   string @ [> 'm @@ many portable | local]) @ [> aliased nonportable] =
  <fun>
|}]

(* The least upper bound between local and global is local *)
let foo (x @ global) =
  let f = which true in
  let y @ global = "global" in
  use_global (f y) (* y is weakened to local before it's applied to f *)
[%%expect{|
Line 4, characters 13-18:
4 |   use_global (f y) (* y is weakened to local before it's applied to f *)
                 ^^^^^
Error: This value is "local" but is expected to be "global".
|}]

let foo (x @ global) =
  let f = which false in
  let y @ global = "global" in
  use_global (f y) (* y is weakened to local before it's applied to f *)
[%%expect{|
Line 4, characters 13-18:
4 |   use_global (f y) (* y is weakened to local before it's applied to f *)
                 ^^^^^
Error: This value is "local" but is expected to be "global".
|}]

(* mode variables used at some mode imposes a bound on them *)
let id x = use_portable x; x
[%%expect{|
val id : 'a @ [< 'm & many portable] -> 'a @ [> 'm | aliased] = <fun>
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
val close_over :
  'a @ [< 'm & global] -> (unit @ 'n -> 'a @ [> 'm]) @ [> close('m)] = <fun>
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
val close_over :
  'a @ [< 'm & global] ->
  (unit @ 'o -> (unit @ 'n -> 'a @ [> 'm]) @ [> close('m)]) @ [> close('m)] =
  <fun>
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
val foo : 'a @ [< global portable] -> unit @ 'm = <fun>
|}]

let foo (x @ portable) =
  use_portable (close_over x)
[%%expect{|
val foo : 'a @ [< global portable] -> unit @ 'm = <fun>
|}]

(* MODE CROSSING *)

(* mode crossing is stronger than mode polymorphism *)
let foo (x : int @ portable) (y : int @ nonportable) =
  let x = id x in
  let y = id y in
  use_portable x;
  use_portable y
[%%expect{|
val foo :
  int @ [< 'm @@ past & portable] ->
  (int @ [> nonportable] -> unit @ 'n) @ [> 'm | nonportable] = <fun>
|}]

(* LOCAL AND MODE POLYMORPHISM *)

(* local values stay local through id - can't return without exclave_ *)
let foo (local_ x) =
  let y = id x in
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
  let y = id x in
  y
[%%expect{|
val foo : 'a @ [< 'm & many portable > local] -> 'a @ [> 'm | local aliased] =
  <fun>
|}]

(* local input stays local through id *)
let foo () =
  let x @ local = "hello" in
  let y = id x in
  use_global y
[%%expect{|
Line 4, characters 13-14:
4 |   use_global y
                 ^
Error: This value is "local" but is expected to be "global".
|}]

(* MULTIPLE MODE AXES *)

(* Bounds can be imposed on multiple axes *)
let foo (x @ global portable) =
  use_global (id x);
  use_portable (id x)
[%%expect{|
val foo : 'a @ [< global many portable] -> unit @ 'm = <fun>
|}]

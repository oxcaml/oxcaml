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

type 'a myref = { mutable i : 'a }
let alloc x = { i = x }
[%%expect{|
type 'a myref = { mutable i : 'a; }
val alloc :
  'a @ [< global many > 'm] -> 'a myref @ [< 'm @@ past > nonportable] =
  <fun>
|}]

(* CR ageorges: make a test case that checks that the codegen of a store
  into a generic mode variable must be [caml_modify_local] if its range
  is [local -- global] *)

(* The code generation of the following needs to use [caml_modify_local] *)
let store_any x y = x.i <- y
[%%expect{|
val store_any :
  'a myref @ [< 'n @@ past & uncontended] ->
  ('a @ [< global many uncontended > 'm] -> unit @ 'o) @ [< 'm @@ past > 'n | nonportable] =
  <fun>
|}]

let store_global (x @ global) y = x.i <- y
[%%expect{|
val store_global :
  'a myref @ [< 'n @@ past & global uncontended] ->
  ('a @ [< global many uncontended > 'm] -> unit @ 'o) @ [< 'm @@ past > 'n | nonportable] =
  <fun>
|}]

let () =
  let (x @ local) = { i = "local" } in
  let (x' @ global) = { i = "global" } in
  store_any x "test";
  store_any x' "test";
  store_global x "should fail"
[%%expect{|
Line 6, characters 15-16:
6 |   store_global x "should fail"
                   ^
Error: This value is "local" but is expected to be "global".
|}]

(* mutable fields are not polymorphic *)
let foo () =
  let x @ unique = alloc 42 in
  let z @ aliased = alloc 42 in
  let yunique = alloc x in
  let yaliased = alloc z in
  use_unique yunique.i;
  use_unique yaliased.i
[%%expect{|
Line 6, characters 13-22:
6 |   use_unique yunique.i;
                 ^^^^^^^^^
Error: This value is "aliased"
         because it is the field "i" (with some modality) of the record at line 6, characters 13-20.
       However, the highlighted expression is expected to be "unique".
|}]

type 'a myrecord = { j : 'a }
let create x = { j = x }
[%%expect{|
type 'a myrecord = { j : 'a; }
val create : 'a @ [< 'm & global] -> 'a myrecord @ [> 'm] = <fun>
|}]

(* but immutable fields are *)
let foo () =
  let x @ unique = create 42 in
  let z @ aliased = create 42 in
  let yunique = create x in
  let yaliased = create z in
  use_unique yunique.j;
  use_unique yaliased.j

[%%expect{|
Line 7, characters 13-23:
7 |   use_unique yaliased.j
                 ^^^^^^^^^^
Error: This value is "aliased"
         because it is the field "j" of the record at line 7, characters 13-21
         which is "aliased".
       However, the highlighted expression is expected to be "unique".
|}]

(* CR ageorges: principality issue with portable refs *)
let foo () =
  use_portable (alloc 42)
[%%expect{|
val foo : unit @ 'n -> unit @ 'm = <fun>
|}, Principal{|
Line 2, characters 15-25:
2 |   use_portable (alloc 42)
                   ^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

let foo (x @ local) = alloc x
[%%expect{|
Line 1, characters 28-29:
1 | let foo (x @ local) = alloc x
                                ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let foo (x @ once) = alloc x
[%%expect{|
Line 1, characters 27-28:
1 | let foo (x @ once) = alloc x
                               ^
Error: This value is "once" but is expected to be "many".
|}]

let foo (x @ contended) = alloc x
[%%expect{|
val foo :
  'a @ [< global many > 'm | contended] ->
  'a myref @ [< 'm @@ past > nonportable contended] = <fun>
|}]

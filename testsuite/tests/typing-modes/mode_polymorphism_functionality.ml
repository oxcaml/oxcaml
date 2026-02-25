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

(* MUTABLE RECORD FIELDS *)

type 'a myref = { mutable i : 'a }
let alloc x = { i = x }
[%%expect{|
type 'a myref = { mutable i : 'a; }
val alloc : 'a @ [< global many] -> 'a myref @ [> nonportable] = <fun>
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
  'a @ [< global many > contended] -> 'a myref @ [> nonportable contended] =
  <fun>
|}]

(* PRODUCTS *)

let prod x y = (x, y)
[%%expect{|
val prod :
  'a @ [< 'o & global] ->
  ('b @ [< 'n & global] -> 'a * 'b @ [> 'n | 'o]) @ 'm = <fun>
|}]

let dupl x = (x, x)
[%%expect{|
val dupl : 'a @ [< 'm & global many] -> 'a * 'a @ [> 'm | aliased] = <fun>
|}]

let foo =
    let p : (unit -> int) @ portable = fun () -> 2 in
    let x = dupl p in
    let snd (_, a) = a in
    use_portable (snd x)
[%%expect{|
val foo : unit = ()
|}]

(* PARTIAL APPLICATION *)

let fst x y = x
[%%expect{|
val fst : 'a @ [< 'n] -> ('b @ 'o -> 'a @ [> 'n]) @ 'm = <fun>
|}]

(* n-ary functions will impose locality bounds on arguments, since the middle end
  must know where to allocate closures of partially applied functions *)

let foo =
  let cglobal = "global" in
  let clocal @ local = "local" in
  let foo = fst cglobal in
  let bar = fst clocal in
  use_global foo;
  use_global bar
[%%expect{|
Line 7, characters 13-16:
7 |   use_global bar
                 ^^^
Error: This value is "local" but is expected to be "global".
|}]

let bar (once_ x) =
  fst x
[%%expect{|
val bar :
  'a @ [< 'n & global > once] ->
  ('b @ 'o -> 'a @ [< 'm > 'm | 'n | once]) @ [< global > once] = <fun>
|}]

let bar (unique_ x) =
  let x = fst x () in
  use_unique x
[%%expect{|
val bar : 'a @ [< global unique] -> unit @ 'm = <fun>
|}]

(* The returned closure is nonportable *)

let () =
  let c = ref 42 in
  let f = fun () -> !c in
  let bar1 = fst f in
  let _ : (unit -> unit) @ portable = fun () -> let _ = bar1 () in () in
  ()
[%%expect{|
Line 5, characters 56-60:
5 |   let _ : (unit -> unit) @ portable = fun () -> let _ = bar1 () in () in
                                                            ^^^^
Error: The value "bar1" is "nonportable"
       but is expected to be "portable"
         because it is used inside the function at line 5, characters 38-69
         which is expected to be "portable".
|}]

let many_arguments x y z s t = y
[%%expect{|
val many_arguments :
  'a @ 'mm3 ->
  ('b @ [< 'q] ->
   ('c @ 'mm2 -> ('d @ 'mm1 -> ('e @ 'mm0 -> 'b @ [> 'q]) @ 'p) @ 'o) @ 'n) @ 'm =
  <fun>
|}]

let foo =
  let x @ local = "local" in
  let y @ global = "global" in
  let y = many_arguments x y () () () in
  use_global y
[%%expect{|
val foo : unit = ()
|}]

let foo =
  let x @ local = "local" in
  let y @ global = "global" in
  let f = many_arguments x y in
  use_global f
[%%expect{|
Line 5, characters 13-14:
5 |   use_global f
                 ^
Error: This value is "local" but is expected to be "global".
|}]

let foo =
  let x @ global = "local" in
  let y @ global = "global" in
  let f = many_arguments x y in
  use_global f
[%%expect{|
val foo : unit = ()
|}]

(* CLOSING OVER POLYMORPHIC MODE VARIABLES *)

(* if [fst] is eta-expanded we observe more useful bounds *)

let fst x = fun y -> x
[%%expect{|
val fst : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [< global] =
  <fun>
|}]

(* x is < global as before *)
let () =
  let c = local_ ref 42 in
  fst c
[%%expect{|
Line 3, characters 6-7:
3 |   fst c
          ^
Error: This value is "local" but is expected to be "global".
|}]

(* once argument yields a once closure *)
let bar (x @ once) =
  let x = (fst x) in
  (x, x)
[%%expect{|
Line 3, characters 6-7:
3 |   (x, x)
          ^
Error: This value is used here,
       but it is defined as once and is also being used at:
Line 3, characters 3-4:
3 |   (x, x)
       ^

|}]

(* unique argument yields unique and once result *)
let bar (x @ unique) =
  let x = fst x in
  use_unique x
[%%expect{|
val bar : 'a @ [< global unique] -> unit @ 'm = <fun>
|}]

let bar (x @ unique) =
  let x = fst x in
  let (f, g) = (x, x) in
  use_unique (f ());
  use_unique (g ())
[%%expect{|
Line 5, characters 14-15:
5 |   use_unique (g ())
                  ^
Error: This value is used here,
       but it is defined as once and has already been used at:
Line 4, characters 14-15:
4 |   use_unique (f ());
                  ^

|}]

(* returned value matches input portability *)
let var (x @ portable) =
  let x = fst x () in
  use_portable x
[%%expect{|
val var : 'a @ [< global portable] -> unit @ 'm = <fun>
|}]
let var (x @ nonportable) =
  let x = fst x () in
  use_portable x
[%%expect{|
Line 3, characters 15-16:
3 |   use_portable x
                   ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* returned value matches input contention *)
let var (x @ uncontended) =
  let x = fst x () in
  use_uncontended x
[%%expect{|
val var : 'a @ [< global uncontended] -> unit @ 'm = <fun>
|}]
let var (x @ contended) =
  let x = fst x () in
  use_uncontended x
[%%expect{|
Line 3, characters 18-19:
3 |   use_uncontended x
                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* The returned closure is nonportable *)
let () =
  let c = ref 42 in
  let f = fun () -> !c in
  let bar1 = fst f in
  let _ : (unit -> unit) @ portable = fun () -> let _ = bar1 () in () in
  ()
[%%expect{|
Line 5, characters 56-60:
5 |   let _ : (unit -> unit) @ portable = fun () -> let _ = bar1 () in () in
                                                            ^^^^
Error: The value "bar1" is "nonportable"
       but is expected to be "portable"
         because it is used inside the function at line 5, characters 38-69
         which is expected to be "portable".
|}]

(* FUCTION APPLICATION *)

(* mode polymorphism is preserved across function applications *)
let id x = x
let id' x = id x
[%%expect{|
val id : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
val id' : 'a @ [< 'm & global] -> 'a @ [> 'm] = <fun>
|}]

let foo (x @ portable) (y @ nonportable) =
  let x = id' x in
  let y = id' y in
  use_portable x;
  use_portable y
[%%expect{|
Line 5, characters 15-16:
5 |   use_portable y
                   ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* HIGHER-ORDER FUNCTIONS *)

let foo (x @ portable) (y @ nonportable) =
  let x = (fun x -> x) x in
  let y = (fun y -> y) y in
  use_portable x;
  use_portable y
[%%expect{|
Line 5, characters 15-16:
5 |   use_portable y
                   ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* higher-order application should propagate mode constraints *)
let apply f = fun x -> f x
[%%expect{|
val apply :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< global] ->
  ('a @ [< 'n] -> 'b @ [> 'm]) @ 'o = <fun>
|}]

(* CR ageorges: the following test ought to succeed; might be due to n-ary function *)
let foo (x @ unique) (y @ aliased) =
  let x = apply id x in
  let y = apply id y in
  use_unique x
[%%expect{|
Line 3, characters 6-7:
3 |   let y = apply id y in
          ^
Warning 26 [unused-var]: unused variable y.

val foo :
  'a @ [< global unique] ->
  ('b @ [< global > aliased] -> unit @ 'm) @ [> once nonportable] = <fun>
|}]

let foo (x @ portable) (y @ nonportable) =
  let id x = x in
  let x = apply id x in
  let y = apply id y in
  use_portable x;
  use_portable y
[%%expect{|
Line 6, characters 15-16:
6 |   use_portable y
                   ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* COMPOSITION *)

let compose f g x = f (g x)
[%%expect{|
val compose :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ 'mm1 ->
  (('c @ [> 'p] -> 'a @ [< 'n & global]) @ 'mm0 ->
   ('c @ [< 'p] -> 'b @ [> 'm]) @ 'q) @ 'o =
  <fun>
|}]

(* mode polymorphism propagates through composition *)

let foo (x @ portable) (y @ nonportable) =
  let x = compose id id x in
  let y = compose id id y in
  use_portable x;
  use_portable y
[%%expect{|
Line 5, characters 15-16:
5 |   use_portable y
                   ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* MODE CROSSING TYPES *)

(* mode crossing is stronger than mode polymorphism *)
let foo (x : int @ portable) (y : int @ nonportable) =
  let x = id x in
  let y = id y in
  use_portable x;
  use_portable y
[%%expect{|
val foo :
  int @ [< portable] ->
  (int @ [> nonportable] -> unit @ 'm) @ [> nonportable] = <fun>
|}]

(* CHAINING APPLICATIONS *)

(* mode constraints propagate through a chain of polymorphic applications *)
let chain x =
  let y = id x in
  let z = id y in
  z
[%%expect{|
val chain : 'a @ [< 'm & global] -> 'a @ [> 'm] = <fun>
|}]

(* CR ageorges: is this the result of uniqueness analysis? *)
let foo (x @ unique) = use_unique (chain x)
[%%expect{|
val foo : 'a @ [< global unique] -> unit @ 'm = <fun>
|}]

let foo (x @ portable) = use_portable (chain x)
[%%expect{|
val foo : 'a @ [< global portable] -> unit @ 'm = <fun>
|}]

(* CLOSING OVER MODE VARIABLES *)

let close_over x = fun () -> x
[%%expect{|
val close_over :
  'a @ [< 'm & global] -> (unit @ 'n -> 'a @ [> 'm]) @ [< global] = <fun>
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
  (unit @ 'o -> (unit @ 'n -> 'a @ [> 'm]) @ [< global]) @ [< global] = <fun>
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

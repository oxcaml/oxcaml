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

(* [fst] is the K-combinator and displays an interesting bi-directional dependency
  between the curry mode, the argument, and the inner return

  Its signature should look like the following:

    val fst : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)]

  Where close('m) refers to the meet between the comonadic parts of 'm and the
  monadic part 'm, taken to its dual comonadic counterpart. Note that 'm describes
  an upper bound in the argument, and a lower bound on the inner return.
*)

let fst x y = x
[%%expect{|
val fst : 'a @ stateless immutable -> 'b -> 'a @ immutable = <fun>
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
Line 5, characters 16-22:
5 |   let bar = fst clocal in
                    ^^^^^^
Error: This value is "local" but is expected to be "global".
|}]

let bar (once_ x) =
  fst x
[%%expect{|
Line 2, characters 6-7:
2 |   fst x
          ^
Error: This value is "once" but is expected to be "many".
|}]

let bar (unique_ x) =
  let x = fst x () in
  use_unique x
[%%expect{|
Line 3, characters 13-14:
3 |   use_unique x
                 ^
Error: This value is "aliased" but is expected to be "unique".
|}]

(* The returned closure is nonportable *)

let () =
  let c = ref 42 in
  let f = fun () -> !c in
  let bar1 = fst f in
  let _ : (unit -> unit) @ portable = fun () -> let _ = bar1 () in () in
  ()
[%%expect{|
Line 4, characters 17-18:
4 |   let bar1 = fst f in
                     ^
Error: This value is "stateful"
         because it contains a usage (of the value "c" at line 3, characters 21-22)
         which is expected to be "read_write".
       However, the highlighted expression is expected to be "stateless".
|}]

let many_arguments x y z s t = y
[%%expect{|
val many_arguments :
  'a @ stateless immutable ->
  'b @ stateless immutable ->
  'c @ stateless immutable ->
  'd @ stateless immutable -> 'e -> 'b @ immutable = <fun>
|}]

let foo =
  let x @ local = "local" in
  let y @ global = "global" in
  let y = many_arguments x y () () () in
  use_global y
[%%expect{|
Line 4, characters 25-26:
4 |   let y = many_arguments x y () () () in
                             ^
Error: This value is "local" but is expected to be "global".
|}]

let foo =
  let x @ local = "local" in
  let y @ global = "global" in
  let f = many_arguments x y in
  use_global f
[%%expect{|
Line 4, characters 25-26:
4 |   let f = many_arguments x y in
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


(* Let's eta-expand [fst] *)

let fst x = fun y -> x
[%%expect{|
val fst : 'a @ stateless -> 'b -> 'a @ immutable = <fun>
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
Line 2, characters 15-16:
2 |   let x = (fst x) in
                   ^
Error: This value is "once" but is expected to be "many".
|}]

(* unique argument yields unique and once result *)
let bar (x @ unique) =
  let x = fst x in
  use_unique x
[%%expect{|
val bar : 'a @ unique stateless -> unit = <fun>
|}]

let bar (x @ unique) =
  let x = fst x in
  let (f, g) = (x, x) in
  use_unique (f ());
  use_unique (g ())
[%%expect{|
Line 4, characters 13-19:
4 |   use_unique (f ());
                 ^^^^^^
Error: This value is "aliased" but is expected to be "unique".
|}]

(* returned value matches input portability *)
let var (x @ portable) =
  let x = fst x () in
  use_portable x
[%%expect{|
Line 3, characters 15-16:
3 |   use_portable x
                   ^
Error: This value is "nonportable" but is expected to be "portable".
|}]
let var (x @ nonportable) =
  let x = fst x () in
  use_portable x
[%%expect{|
Line 2, characters 14-15:
2 |   let x = fst x () in
                  ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* returned value matches input contention *)
let var (x @ uncontended) =
  let x = fst x () in
  use_uncontended x
[%%expect{|
Line 3, characters 18-19:
3 |   use_uncontended x
                      ^
Error: This value is "immutable" but is expected to be "read_write".
|}]
let var (x @ contended) =
  let x = fst x () in
  use_uncontended x
[%%expect{|
Line 2, characters 14-15:
2 |   let x = fst x () in
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
Line 4, characters 17-18:
4 |   let bar1 = fst f in
                     ^
Error: This value is "stateful"
         because it contains a usage (of the value "c" at line 3, characters 21-22)
         which is expected to be "read_write".
       However, the highlighted expression is expected to be "stateless".
|}]

(* deeply nested closures should still propagate modes *)
let nest x = fun () -> fun () -> fun () -> x
[%%expect{|
val nest : 'a @ stateless -> unit -> unit -> unit -> 'a @ immutable = <fun>
|}]

let foo (x @ portable) =
  use_portable (nest x () () ())
[%%expect{|
Line 2, characters 15-32:
2 |   use_portable (nest x () () ())
                   ^^^^^^^^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

let foo (x @ nonportable) =
  use_portable (nest x () () ())
[%%expect{|
Line 2, characters 21-22:
2 |   use_portable (nest x () () ())
                         ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

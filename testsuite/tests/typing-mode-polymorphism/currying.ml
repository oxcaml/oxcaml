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
val fst : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] = <fun>
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
  'a @ [< 'm & global > once] ->
  ('b @ [< 'o @@ past > 'o] -> 'a @ [< 'n > 'n | 'm | once]) @ [> close('m) | once] =
  <fun>
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
  'a @ [< 'mm2 @@ past & 'q @@ past & 'o @@ past & 'm @@ past] ->
  ('b @ [< 'n] ->
   ('c @ [< 'mm1 @@ past & 'p @@ past] ->
    ('d @ [< 'mm0 @@ past] ->
     ('e @ 'mm3 -> 'b @ [> 'n]) @ [> close('n) | 'mm0 | 'mm1 | 'mm2]) @ [> close('n) | 'p | 'q]) @ [> close('n) | 'o]) @ [> 'm] =
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


(* Let's eta-expand [fst] *)

let fst x = fun y -> x
[%%expect{|
val fst : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] =
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

(* deeply nested closures should still propagate modes *)
let nest x = fun () -> fun () -> fun () -> x
[%%expect{|
val nest :
  'a @ [< 'm & global] ->
  (unit @ 'p ->
   (unit @ 'o -> (unit @ 'n -> 'a @ [> 'm]) @ [> close('m)]) @ [> close('m)]) @ [> close('m)] =
  <fun>
|}]

let foo (x @ portable) =
  use_portable (nest x () () ())
[%%expect{|
val foo : 'a @ [< global portable] -> unit @ 'm = <fun>
|}]

let foo (x @ nonportable) =
  use_portable (nest x () () ())
[%%expect{|
Line 2, characters 15-32:
2 |   use_portable (nest x () () ())
                   ^^^^^^^^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

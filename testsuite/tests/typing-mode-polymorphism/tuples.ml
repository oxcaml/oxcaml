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

(* Since a tuple is returned in tail-position, its allocation will be global *)
let prod x y = (x, y)
[%%expect{|
val prod : 'a @ stateless immutable -> 'b @ stateless -> 'a * 'b @ immutable =
  <fun>
|}]

(* With exclave_ the tuple is local *)
let prod_local x y = exclave_ (x, y)
[%%expect{|
val prod_local :
  'a @ stateless immutable -> 'b @ stateless -> 'a * 'b @ local immutable =
  <fun>
|}]

(* [prod] is polymorphic on the other axes *)
let foo (x @ portable) (y @ portable) =
  let (b, a) = prod x y in
  use_portable b;
  use_portable a
[%%expect{|
Line 3, characters 15-16:
3 |   use_portable b;
                   ^
Error: This value is "nonportable"
         because it is an element of the tuple at line 2, characters 15-23
         which is "nonportable".
       However, the highlighted expression is expected to be "portable".
|}]

(* But the returned tuple will be the meet of its arguments *)
let foo (x @ portable) (y @ nonportable) =
  let (b, a) = prod x y in
  use_portable b
[%%expect{|
Line 2, characters 22-23:
2 |   let (b, a) = prod x y in
                          ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

let dupl x = (x, x)
[%%expect{|
val dupl : 'a @ stateless -> 'a * 'a @ immutable = <fun>
|}]

(* The arguments must be global *)
let foo (x @ local) (y @ global) =
    let _ = prod x y in ()
[%%expect{|
Line 2, characters 17-18:
2 |     let _ = prod x y in ()
                     ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let foo (y @ local) (x @ global) =
    let _ = prod y x in ()
[%%expect{|
Line 2, characters 17-18:
2 |     let _ = prod y x in ()
                     ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

(* They can be local using exclave_ *)
let foo (x @ local) (y @ local) =
  let p = prod_local x y in
  use_global (fst p) (* but the elements of the product are local *)
[%%expect{|
Line 2, characters 21-22:
2 |   let p = prod_local x y in
                         ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

(* [dupl] uses an argument twice and the polymorphic mode must be many *)
let foo (x @ once) =
  let p = dupl x in ()
[%%expect{|
Line 2, characters 15-16:
2 |   let p = dupl x in ()
                   ^
Error: This value is "once" but is expected to be "many".
|}]

(* [dupl] makes unique arguments aliased *)
let foo (x @ unique) =
  let p = dupl x in
  use_unique (fst p)
[%%expect{|
Line 3, characters 18-19:
3 |   use_unique (fst p)
                      ^
Error: This value is "immutable" but is expected to be "read_write".
|}]

(* mode polymorphism works over tuples *)
let swap (a, b) = (b, a)
[%%expect{|
val swap : 'a * 'b @ stateless -> 'b * 'a @ immutable = <fun>
|}]

let swap_local (a, b) = exclave_ (b, a)
[%%expect{|
val swap_local : 'a * 'b @ stateless -> 'b * 'a @ local immutable = <fun>
|}]

let foo (x @ portable) (y @ portable) =
  let p = swap (x, y) in
  use_portable p
[%%expect{|
Line 3, characters 15-16:
3 |   use_portable p
                   ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

let foo (x @ local) (y @ local) =
  let _ = swap (x, y) in ()
[%%expect{|
Line 2, characters 16-17:
2 |   let _ = swap (x, y) in ()
                    ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is an element of the tuple at line 2, characters 15-21
         which is expected to be "global".
|}]

let foo (x @ global) (y @ local) =
  let p = swap_local (x, y) in
  use_global (snd p)
[%%expect{|
Line 2, characters 25-26:
2 |   let p = swap_local (x, y) in
                             ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is an element of the tuple at line 2, characters 21-27
         which is expected to be "global".
|}]

let foo (x @ global) (y @ global) =
  let p = swap_local (x, y) in
  use_global p
[%%expect{|
Line 3, characters 13-14:
3 |   use_global p
                 ^
Error: This value is "local" but is expected to be "global".
|}]

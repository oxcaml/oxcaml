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
val use_uncontended : 'a @ [< uncontended] -> unit @ [< global] = <fun>
val use_portable : 'a @ [< portable] -> unit @ [< global] = <fun>
val use_unique : 'a @ [< unique] -> unit @ [< global] = <fun>
val use_static : 'a @ 'm -> unit @ [< global] = <fun>
val use_global : 'a @ [< global] -> unit @ [< global] = <fun>
|}]

(* Since a tuple is returned in tail-position, its allocation will be global *)
let prod x y = (x, y)
[%%expect{|
val prod :
  'a @ [< 'p & 'n.future & global] ->
  ('b @ [< 'o & global] -> 'a * 'b @ [< global > 'm | 'o | 'p]) @ [< global > close('m) | 'n.future] =
  <fun>
|}]

(* With exclave_ the tuple is local *)
let prod_local (x @ local) (y @ local) = exclave_ (x, y)
[%%expect{|
val prod_local :
  'a @ [< 'p & 'n.future > local] ->
  ('b @ [< 'o > local] -> 'a * 'b @ [> 'm | 'o | 'p | local]) @ [> close('m) | 'n.future | local] =
  <fun>
|}]

(* [prod] is polymorphic on the other axes *)
let foo (x @ portable) (y @ portable) =
  let (b, a) = prod x y in
  use_portable b;
  use_portable a
[%%expect{|
val foo :
  'a @ [< 'm.future & global portable] ->
  ('b @ [< global portable] -> unit @ [< global]) @ [< global > 'm.future] =
  <fun>
|}]

(* But the returned tuple will be the meet of its arguments *)
let foo (x @ portable) (y @ nonportable) =
  let (b, a) = prod x y in
  use_portable b
[%%expect{|
Line 3, characters 15-16:
3 |   use_portable b
                   ^
Error: This value is "nonportable"
         because it is an element of the tuple at line 2, characters 15-23
         which is "nonportable".
       However, the highlighted expression is expected to be "portable".
|}]

let dupl x = (x, x)
[%%expect{|
val dupl : 'a @ [< 'm & global many] -> 'a * 'a @ [< global > 'm | aliased] =
  <fun>
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
Line 3, characters 13-20:
3 |   use_global (fst p) (* but the elements of the product are local *)
                 ^^^^^^^
Error: This value is "local" but is expected to be "global".
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
Line 3, characters 13-20:
3 |   use_unique (fst p)
                 ^^^^^^^
Error: This value is "aliased" but is expected to be "unique".
|}]

(* mode polymorphism works over tuples *)
let swap (a, b) = (b, a)
[%%expect{|
val swap : 'a * 'b @ [< 'm & global] -> 'b * 'a @ [< global > 'm] = <fun>
|}]

let swap_local (a, b) = exclave_ (b, a)
[%%expect{|
val swap_local : 'a * 'b @ [< 'm] -> 'b * 'a @ [> 'm | local] = <fun>
|}]

let foo (x @ portable) (y @ portable) =
  let p = swap (x, y) in
  use_portable p
[%%expect{|
val foo :
  'a @ [< 'm.future & global portable] ->
  ('b @ [< global portable] -> unit @ [< global]) @ [< global > 'm.future] =
  <fun>
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
Line 3, characters 13-20:
3 |   use_global (snd p)
                 ^^^^^^^
Error: This value is "local" but is expected to be "global".
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

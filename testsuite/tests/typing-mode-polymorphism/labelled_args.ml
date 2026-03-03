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

let id ~label1 = label1
[%%expect{|
val id : label1:'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

let () =
  let (x @ uncontended) = "uncontended" in
  let y = id ~label1:x in
  use_uncontended y
[%%expect{|
|}]

let fst ~label1 ~label2 = label1
[%%expect{|
val fst :
  label1:'a @ [< 'o & 'n.future] ->
  (label2:'b @ 'p -> 'a @ [> 'm | 'o]) @ [> close('m) | 'n.future] = <fun>
|}]

let () =
  let (x @ uncontended) = "uncontended" in
  let f = fst ~label1:x in
  use_uncontended (f ~label2:())
[%%expect{|
|}]

let () =
  let (x @ uncontended) = "uncontended" in
  let f = fst ~label2:() in
  use_uncontended (f ~label1:x)
[%%expect{|
|}]

let () =
  let (x @ global) = "global" in
  let y = fst ~label1:x ~label2:() in
  use_global y
[%%expect{|
|}]

let () =
  let (x @ local) = "local" in
  let y = fst ~label1:x ~label2:() in
  use_global y
[%%expect{|
Line 4, characters 13-14:
4 |   use_global y
                 ^
Error: This value is "local" but is expected to be "global".
|}]

let () =
  let (x @ global) = "global" in
  let y = fst ~label2:() ~label2:x in
  use_global y
[%%expect{|
|}]

let () =
  let (x @ local) = "local" in
  let y = fst ~label2:() ~label2:x in
  use_global y
[%%expect{|
Line 4, characters 13-14:
4 |   use_global y
                 ^
Error: This value is "local" but is expected to be "global".
|}]

let snd ~label1 ~label2 = label2
[%%expect{|
val snd :
  label1:'a @ [< 'm.future] ->
  (label2:'b @ [< 'n] -> 'b @ [> 'n]) @ [> 'm.future] = <fun>
|}]

let () =
  let (x @ uncontended) = "uncontended" in
  let f = snd ~label2:x in
  use_uncontended (f ~label1:())
[%%expect{|
|}]

let foo = fst ~label2:()
[%%expect{|
val foo : label1:'a @ [< 'n & 'n] -> 'a @ [< 'm > 'm | 'n | 'n] = <fun>
|}]

(* partially applying a labelled function yield
the correct close-over behavior *)
(* [foo] closes over a local variable and should thus be local *)
let () =
  let (label2 @ local) = "local" in
  let foo = fst ~label2 in
  use_global foo
[%%expect{|
Line 4, characters 13-16:
4 |   use_global foo
                 ^^^
Error: This value is "local" but is expected to be "global".
|}]

let () =
  let label2 = "global" in
  let foo = fst ~label2 in
  use_global foo
[%%expect{|
|}]

let foo = fun x -> fst ~label1:x
[%%expect{|
val foo :
  'a @ [< 'o & 'n.future & global] ->
  (label2:'b @ 'p -> 'a @ [> 'm | 'o]) @ [> close('m) | 'n.future] = <fun>
|}]

let foo ?label1 x = x
[%%expect{|
val foo :
  ?label1:'a @ [< 'm.future] -> ('b @ [< 'n] -> 'b @ [> 'n]) @ [> 'm.future] =
  <fun>
|}]

let () =
  let (x @ global) = "global" in
  let y = foo x in
  use_global y
[%%expect{|
|}]

let () =
  let (x @ local) = "local" in
  let y = foo x in
  use_global y
[%%expect{|
Line 4, characters 13-14:
4 |   use_global y
                 ^
Error: This value is "local" but is expected to be "global".
|}]

let foo x ?label1 = x
[%%expect{|
val foo :
  'a @ [< 'o & 'n.future] ->
  (?label1:'b @ 'p -> 'a @ [> 'm | 'o]) @ [> close('m) | 'n.future] = <fun>
|}]

let () =
  let (x @ global) = "global" in
  let y = foo x in
  use_global y
[%%expect{|
|}]

let () =
  let (x @ local) = "local" in
  let y = foo x in
  use_global y
[%%expect{|
Line 4, characters 13-14:
4 |   use_global y
                 ^
Error: This value is "local" but is expected to be "global".
|}]

let bar (x @ nonportable) =
  let f = foo x in
  use_portable f
[%%expect{|
Line 3, characters 15-16:
3 |   use_portable f
                   ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

let bar (x @ uncontended) =
  let f = foo x in
  let y = f ~label1:() in
  use_portable y
[%%expect{|
val bar : 'a @ [< global portable uncontended] -> unit @ 'm = <fun>
|}]

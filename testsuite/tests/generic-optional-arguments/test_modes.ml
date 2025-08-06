(* TEST
 flags = "-extension-universe alpha";
 expect;
*)


type 'a t =
  | None
  | Some of 'a @@ global
[@@option_like]
[%%expect{|
type 'a t = None | Some of global_ 'a
|}]

let h ?(x : 'a @ local) () = x
let v = h ~x:3 ()
[%%expect{|
val h : ?x:local_ 'a -> unit -> local_ 'a option = <fun>
Line 2, characters 8-17:
2 | let v = h ~x:3 ()
            ^^^^^^^^^
Error: This value escapes its region.
|}]
let h ?(x : 'a @ local = 2) () = x
let v = h ~x:3 ()
[%%expect{|
val h : ?x:local_ int -> unit -> int = <fun>
val v : int = 3
|}, Principal{|
val h : ?x:local_ int -> unit -> local_ int = <fun>
val v : int = 3
|}]
let h ?(x : 'a @ local = "2") () = x
let v = h ~x:"3" ()
[%%expect{|
val h : ?x:local_ string -> unit -> local_ string = <fun>
Line 2, characters 8-19:
2 | let v = h ~x:"3" ()
            ^^^^^^^^^^^
Error: This value escapes its region.
|}]


let f (?x : 'a t @ local) () = x
[%%expect{|
val f : (?x):local_ 'a t -> unit -> local_ 'a t = <fun>
|}]

let g (?(x = 2) : _ t @ local) () = x
let v = g()
let v = g ~x:3 ()
let v = g ?x:(Some 3) ()
let v = g ?x:(None) ()
[%%expect{|
val g : (?x):local_ int t -> unit -> int = <fun>
val v : int = 2
val v : int = 3
val v : int = 3
val v : int = 2
|}, Principal{|
val g : (?x):local_ int t -> unit -> local_ int = <fun>
val v : int = 2
val v : int = 3
val v : int = 3
val v : int = 2
|}]

(* CR generic-optional: this test should succeed once the pattern is interpreted
   semantically *)
let g (?(x = "2") : _ t @ local) () = (x : string @ global)
let v = g()
let v = g ~x:"3" ()
let v = g ?x:(Some "3") ()
let v = g ?x:(None) ()
[%%expect{|
Line 1, characters 39-40:
1 | let g (?(x = "2") : _ t @ local) () = (x : string @ global)
                                           ^
Error: This value escapes its region.
|}]
(* also an error if only modes are provided without a type *)
let f (?x @ local) () = x

[%%expect{|
Line 1, characters 8-17:
1 | let f (?x @ local) () = x
            ^^^^^^^^^
Error: Generic optional arguments require a type annotation
|}]

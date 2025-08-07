(* TEST
 flags = "-extension-universe alpha";
 expect;
*)


type 'a t = 'a or_null [@@or_null_reexport]
type 'a p = 'a t [@@or_null_reexport]
[%%expect {|
type 'a t = 'a or_null = Null | This of 'a [@@or_null_reexport]
type 'a p = 'a t = Null | This of 'a [@@or_null_reexport]
|}]

(* CR generic-optional: This should work once we have semantic interpretation *)
let f (?x : 'a t) () = x
let v = f ()
let v = f ~x:3 ()
let v = f ?x:(This 3) ()
let v = f ?x:Null ()
let g (?(x = 2) : _ p) () = x
let v = g()
let v = g ~x:3 ()
let v = g ?x:(This 3) ()
let v = g ?x:(Null) ()
[%%expect{|
val f : (?x):'a t -> unit -> 'a t = <fun>
val v : 'a t = Null
val v : int t = This 3
val v : int t = This 3
val v : 'a t = Null
val g : (?x):int or_null -> unit -> int = <fun>
val v : int = 2
val v : int = 3
val v : int = 3
val v : int = 2
|}]

let v = f ?x:(Some 3) ()
let v = f ?x:None ()
[%%expect {|
Line 1, characters 14-18:
1 | let v = f ?x:(Some 3) ()
                  ^^^^
Error: This variant expression is expected to have type "'a t"
       There is no constructor "Some" within type "t"
|}]

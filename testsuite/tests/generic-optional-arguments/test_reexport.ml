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
let v = f ?x:(Some 3) ()
let v = f ?x:(None) ()
let g (?(x = 2) : _ p) () = x
let v = g()
let v = g ~x:3 ()
let v = g ?x:(Some 3) ()
let v = g ?x:(None) ()
[%%expect{|
Line 1, characters 12-16:
1 | let f (?x : 'a t) () = x
                ^^^^
Error: Generic optional arguments require types with the [@option_like] attribute.
       Type "t" is not marked as option-like
|}]
[%%expect {|
|}]

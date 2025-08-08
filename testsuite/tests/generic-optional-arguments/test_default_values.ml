(* TEST
flags = "-extension-universe alpha ";

 expect;
*)

let g (?(x = 42) : int option) () = x
let v = g ()
[%%expect {|
val g : (?x):int option -> unit -> int = <fun>
val v : int = 42
|}]

let g (?(x = "s") : int option) () = x
let v = g ()
[%%expect {|
Line 1, characters 13-16:
1 | let g (?(x = "s") : int option) () = x
                 ^^^
Error: This expression has type "string" but an expression was expected of type
         "int"
|}]

let g (?(x = 42) : option option) () = x
let v = g ()
[%%expect {|
Line 1, characters 19-25:
1 | let g (?(x = 42) : option option) () = x
                       ^^^^^^
Error: The type constructor "option" expects 1 argument(s),
       but is here applied to 0 argument(s)
|}]

let g (?(x = (1, 2)) : (int * int) option) () = x
let v = g ()
[%%expect {|
val g : (?x):(int * int) option -> unit -> int * int = <fun>
val v : int * int = (1, 2)
|}]
let g (?(x = (1, "2")) : (int * int) option) () = x
let v = g ()
[%%expect {|
Line 1, characters 17-20:
1 | let g (?(x = (1, "2")) : (int * int) option) () = x
                     ^^^
Error: This expression has type "string" but an expression was expected of type
         "int"
|}]

let g (?x:(y = 42) : int option) () = y
let v = g ()
[%%expect {|
val g : (?x):int option -> unit -> int = <fun>
val v : int = 42
|}]

let g (?x:(y = "s") : int option) () = y
let v = g ()
[%%expect {|
Line 1, characters 15-18:
1 | let g (?x:(y = "s") : int option) () = y
                   ^^^
Error: This expression has type "string" but an expression was expected of type
         "int"
|}]


let g (?x:((y,z) = (1, 2)) : (int * int) option) () = y
let v = g ()
[%%expect {|
val g : (?x):(int * int) option -> unit -> int = <fun>
val v : int = 1
|}]
let g (?x:((y,z) = (1, "2")) : (int * int) option) () = y
let v = g ()
[%%expect {|
Line 1, characters 23-26:
1 | let g (?x:((y,z) = (1, "2")) : (int * int) option) () = y
                           ^^^
Error: This expression has type "string" but an expression was expected of type
         "int"
|}]
let g (?x:((y, z, w) = (1, 2)) : (int * int) option) () = y
let v = g ()
[%%expect {|
Line 1, characters 11-20:
1 | let g (?x:((y, z, w) = (1, 2)) : (int * int) option) () = y
               ^^^^^^^^^
Error: This pattern was expected to match values of type "int * int",
       but it contains an extra unlabeled component.
|}, Principal{|
Line 1, characters 11-20:
1 | let g (?x:((y, z, w) = (1, 2)) : (int * int) option) () = y
               ^^^^^^^^^
Error: This pattern matches values of type "'a * 'b * 'c"
       but a pattern was expected which matches values of type "int * int"
|}]

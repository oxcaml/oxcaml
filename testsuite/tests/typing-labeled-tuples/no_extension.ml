(* TEST
   flags = "-extension-universe no_extensions";
   expect;
*)

(* Labeled tuples were added upstream in 5.4, so all this should now work
   without extensions. *)
type t = x:int * y:string
let f x y = ~x, ~y
let x = ~x:5, ~y:"hi"
let f x y = ~(x : int), ~(y : string)
let f (~x, ~y) = x, y
let f (~x:_, ~y:_) = ()
let f (~(x:int), ~(y:string)) = x, y
let (_ : x : int * y : string) = assert false

[%%expect{|
type t = x:int * y:string
val f : 'a -> 'b -> x:'a * y:'b = <fun>
val x : x:int * y:string = (~x:5, ~y:"hi")
val f : int -> string -> x:int * y:string = <fun>
val f : (x:'a * y:'b) -> 'a * 'b = <fun>
val f : (x:'a * y:'b) -> unit = <fun>
val f : (x:int * y:string) -> int * string = <fun>
Exception: Assert_failure ("", 8, 33).
|}]

let (_ : ('a -> x : int * y : string) -> 'b) = assert false

[%%expect{|
Exception: Assert_failure ("", 1, 47).
|}]

let f ((x, ..) : (int * string * float)) = x
let f ((x, y, ..) : (int * string * float)) = x, y

[%%expect{|
val f : int * string * float -> int = <fun>
val f : int * string * float -> int * string = <fun>
|}]

(* However, upstream rejected _repeated_ labels, so these tests are adapted to
   use two identical labels which should require extensions. *)

type t = x:int * x:string

[%%expect{|
Line 1, characters 9-25:
1 | type t = x:int * x:string
             ^^^^^^^^^^^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]

let f x y = ~x, ~x:y

[%%expect{|
Line 1, characters 12-20:
1 | let f x y = ~x, ~x:y
                ^^^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]

let x = ~x:5, ~x:"hi"

[%%expect{|
Line 1, characters 8-21:
1 | let x = ~x:5, ~x:"hi"
            ^^^^^^^^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]

let f x y = ~(x : int), ~x:(y : string)

[%%expect{|
Line 1, characters 12-39:
1 | let f x y = ~(x : int), ~x:(y : string)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]

let f (~x, ~x:y) = x, y

[%%expect{|
Line 1, characters 6-16:
1 | let f (~x, ~x:y) = x, y
          ^^^^^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]

let f (~x:_, ~x:_) = ()

[%%expect{|
Line 1, characters 6-18:
1 | let f (~x:_, ~x:_) = ()
          ^^^^^^^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]

let f (~(x:int), ~x:(y:string)) = x, y

[%%expect{|
Line 1, characters 6-31:
1 | let f (~(x:int), ~x:(y:string)) = x, y
          ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]

let (_ : x : int * x : string) = assert false

[%%expect{|
Line 1, characters 9-29:
1 | let (_ : x : int * x : string) = assert false
             ^^^^^^^^^^^^^^^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]

let (_ : ('a -> x : int * x : string) -> 'b) = assert false

[%%expect{|
Line 1, characters 16-36:
1 | let (_ : ('a -> x : int * x : string) -> 'b) = assert false
                    ^^^^^^^^^^^^^^^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]

(* TEST
 flags = "-dsource";
 expect;
*)
let x = ~x:1, ~y:2
[%%expect{|

let x = (~x:1, ~y:2);;
val x : x:int * y:int = (~x:1, ~y:2)
|}]

(* Attribute should prevent punning *)
let z = 5
let y = ~z:z, ~z, ~z:(z [@attr])
[%%expect{|

let z = 5;;
val z : int = 5

let y = (~z, ~z, ~z:((z)[@attr ]));;
Line 2, characters 8-32:
2 | let y = ~z:z, ~z, ~z:(z [@attr])
            ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Label "~z" occurs more than once in this tuple type
|}]

let (~x:x0, ~s, ~(y:int), ..) : x:int * s:string * y:int * string =
   ~x: 1, ~s: "a", ~y: 2, "ignore me"
[%%expect{|

let (~x:x0, ~s, ~y:(y : int), ..) : (x:int * s:string * y:int * string) =
  (~x:1, ~s:"a", ~y:2, "ignore me");;
val x0 : int = 1
val s : string = "a"
val y : int = 2
|}]

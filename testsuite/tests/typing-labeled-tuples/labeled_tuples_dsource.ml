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
let z1 = 5
let z2 = 5
let z3 = 5
let y = ~z1:z1, ~z2, ~z3:(z3 [@attr])
[%%expect{|

let z1 = 5;;
val z1 : int = 5

let z2 = 5;;
val z2 : int = 5

let z3 = 5;;
val z3 : int = 5

let y = (~z1, ~z2, ~z3:((z3)[@attr ]));;
val y : z1:int * z2:int * z3:int = (~z1:5, ~z2:5, ~z3:5)
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

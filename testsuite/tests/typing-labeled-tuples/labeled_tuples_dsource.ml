(* TEST
<<<<<<< HEAD
 flags = "-extension labeled_tuples -dsource";
 expect;
=======
   flags += "-dsource";
   expect;
>>>>>>> upstream/5.4
*)
let x = ~x:1, ~y:2
[%%expect{|

let x = (~x:1, ~y:2);;
val x : x:int * y:int = (~x:1, ~y:2)
|}]

(* Attribute should prevent punning *)
let z = 5
<<<<<<< HEAD
let y = ~z:z, ~z, ~z:(z [@attr])
=======
let y = ~z, ~z':z, ~z1:(z [@attr])
>>>>>>> upstream/5.4
[%%expect{|

let z = 5;;
val z : int = 5

<<<<<<< HEAD
let y = (~z, ~z, ~z:((z)[@attr ]));;
val y : z:int * z:int * z:int = (~z:5, ~z:5, ~z:5)
=======
let y = (~z, ~z':z, ~z1:((z)[@attr ]));;
val y : z:int * z':int * z1:int = (~z:5, ~z':5, ~z1:5)
>>>>>>> upstream/5.4
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

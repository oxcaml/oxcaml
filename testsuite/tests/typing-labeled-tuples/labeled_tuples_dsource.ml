(* TEST
<<<<<<< HEAD
   flags += "-dsource";
   expect;
||||||| eb63e0e418
 flags = "-extension labeled_tuples -dsource";
 expect;
=======
 flags = "-dsource";
 expect;
>>>>>>> dd4e8507373d22fb295422eb6dd3d997c76c47cb
*)
let x = ~x:1, ~y:2
[%%expect{|

let x = (~x:1, ~y:2);;
val x : x:int * y:int = (~x:1, ~y:2)
|}]

(* Attribute should prevent punning *)
<<<<<<< HEAD
let z = 5
let y = ~z, ~z':z, ~z1:(z [@attr])
||||||| eb63e0e418
let z = 5
let y = ~z:z, ~z, ~z:(z [@attr])
=======
let z1 = 5
let z2 = 5
let z3 = 5
let y = ~z1:z1, ~z2, ~z3:(z3 [@attr])
>>>>>>> dd4e8507373d22fb295422eb6dd3d997c76c47cb
[%%expect{|

let z1 = 5;;
val z1 : int = 5

<<<<<<< HEAD
let y = (~z, ~z':z, ~z1:((z)[@attr ]));;
val y : z:int * z':int * z1:int = (~z:5, ~z':5, ~z1:5)
||||||| eb63e0e418
let y = (~z, ~z, ~z:((z)[@attr ]));;
val y : z:int * z:int * z:int = (~z:5, ~z:5, ~z:5)
=======
let z2 = 5;;
val z2 : int = 5

let z3 = 5;;
val z3 : int = 5

let y = (~z1, ~z2, ~z3:((z3)[@attr ]));;
val y : z1:int * z2:int * z3:int = (~z1:5, ~z2:5, ~z3:5)
>>>>>>> dd4e8507373d22fb295422eb6dd3d997c76c47cb
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

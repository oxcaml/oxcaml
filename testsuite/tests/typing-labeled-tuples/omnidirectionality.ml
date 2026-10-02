(* TEST
   expect;
*)

let apply f x = f x
let unify x y = ignore (x = y)
[%%expect {|
val apply : ('a -> 'b) -> 'a -> 'b = <fun>
val unify : 'a -> 'a -> unit = <fun>
|}];;

let a1 t = t.~x;;
[%%expect {|
Line 1, characters 11-15:
1 | let a1 t = t.~x;;
               ^^^^
Error: The type of the tuple express is ambiguous.
       Could not determine the type of the tuple projection.
|}];;

let b1 = (fun t -> t.~x) (~x:1, ~y:2);;
[%%expect {|
val b1 : int = 1
|}, Principal{|
Line 1, characters 19-23:
1 | let b1 = (fun t -> t.~x) (~x:1, ~y:2);;
                       ^^^^
Error: The type of the tuple express is ambiguous.
       Could not determine the type of the tuple projection.
|}];;

let b2 = apply (fun t -> t.~x) (~x:1, ~y:2);;
[%%expect {|
val b2 : int = 1
|}];;

let b3 = (~x:1, ~y:2) |> (fun t -> t.~x);;
[%%expect {|
val b3 : int = 1
|}];;

let b4 t = if true then t.~x else (unify t (~x:1, ~y:2); 0);;
[%%expect {|
val b4 : (x:int * y:int) -> int = <fun>
|}, Principal{|
Line 1, characters 24-28:
1 | let b4 t = if true then t.~x else (unify t (~x:1, ~y:2); 0);;
                            ^^^^
Error: The type of the tuple express is ambiguous.
       Could not determine the type of the tuple projection.
|}];;

let b5 = (fun t -> (t.~x, t.~y)) (~y:"a", 1, ~x:true);;
[%%expect {|
val b5 : bool * string = (true, "a")
|}, Principal{|
Line 1, characters 26-30:
1 | let b5 = (fun t -> (t.~x, t.~y)) (~y:"a", 1, ~x:true);;
                              ^^^^
Error: The type of the tuple express is ambiguous.
       Could not determine the type of the tuple projection.
|}];;

let b6 = (fun t -> t.~a.~b) (~a:(~b:1, ~c:2), ~d:3);;
[%%expect {|
val b6 : int = 1
|}, Principal{|
Line 1, characters 19-26:
1 | let b6 = (fun t -> t.~a.~b) (~a:(~b:1, ~c:2), ~d:3);;
                       ^^^^^^^
Error: The type of the tuple express is ambiguous.
       Could not determine the type of the tuple projection.
|}];;

(* ocaml/ocaml#14257 *)
let b7 = List.map (fun t -> t.~x) [~x:1, ~y:2];;
[%%expect {|
val b7 : int list = [1]
|}, Principal{|
Line 1, characters 28-32:
1 | let b7 = List.map (fun t -> t.~x) [~x:1, ~y:2];;
                                ^^^^
Error: The type of the tuple express is ambiguous.
       Could not determine the type of the tuple projection.
|}];;

let c1 t = let r = t.~x in unify t (~x:1, ~y:2); r;;
[%%expect {|
Line 1, characters 19-23:
1 | let c1 t = let r = t.~x in unify t (~x:1, ~y:2); r;;
                       ^^^^
Error: The type of the tuple express is ambiguous.
       Could not determine the type of the tuple projection.
|}];;

let c2 t = ignore t.~x; (t : x:int * y:int);;
[%%expect {|
Line 1, characters 18-22:
1 | let c2 t = ignore t.~x; (t : x:int * y:int);;
                      ^^^^
Error: The type of the tuple express is ambiguous.
       Could not determine the type of the tuple projection.
|}];;

let c3 = (fun t -> t.~x + 1) (~x:1, ~y:2);;
[%%expect {|
Line 1, characters 19-23:
1 | let c3 = (fun t -> t.~x + 1) (~x:1, ~y:2);;
                       ^^^^
Error: The type of the tuple express is ambiguous.
       Could not determine the type of the tuple projection.
|}];;

let c4 = let f t = t.~x in f (~x:1, ~y:2);;
[%%expect {|
Line 1, characters 19-23:
1 | let c4 = let f t = t.~x in f (~x:1, ~y:2);;
                       ^^^^
Error: The type of the tuple express is ambiguous.
       Could not determine the type of the tuple projection.
|}];;

let e1 = (fun t -> t.~z) (~x:1, ~y:2);;
[%%expect {|
Line 1, characters 19-23:
1 | let e1 = (fun t -> t.~z) (~x:1, ~y:2);;
                       ^^^^
Error: No field "z" for the tuple "x:int * y:int".
|}, Principal{|
Line 1, characters 19-23:
1 | let e1 = (fun t -> t.~z) (~x:1, ~y:2);;
                       ^^^^
Error: The type of the tuple express is ambiguous.
       Could not determine the type of the tuple projection.
|}];;

let e2 = (fun t -> t.~x) 1;;
[%%expect {|
Line 1, characters 19-23:
1 | let e2 = (fun t -> t.~x) 1;;
                       ^^^^
Error: This expression has type "int" which is not a tuple.
|}, Principal{|
Line 1, characters 19-23:
1 | let e2 = (fun t -> t.~x) 1;;
                       ^^^^
Error: The type of the tuple express is ambiguous.
       Could not determine the type of the tuple projection.
|}];;

(* Delayed mode crossing *)
let m1 (local_ t : x:int * y:string) = t.~x;;
[%%expect {|
val m1 : (x:int * y:string) @ local -> int = <fun>
|}, Principal{|
val m1 : (x:int * y:string) @ local -> int @ local = <fun>
|}];;

let m2 (local_ t : x:int * y:string) = t.~y;;
[%%expect {|
val m2 : (x:int * y:string) @ local -> string @ local = <fun>
|}];;

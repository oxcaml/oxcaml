(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(* local is different from <[local]> *)
let f (x @ local quote_local) = x
[%%expect{|
val f : 'a @ local quote_local -> 'a @ local quote_local = <fun>
|}];;

(* The ambient state monad requires <[global]> *)

let set r (x @ quote_local) = r := x
[%%expect{|
Line 1, characters 35-36:
1 | let set r (x @ quote_local) = r := x
                                       ^
Error: This value is "quote_local" but is expected to be "quote_global".
|}];;

exception Extrusion of <[int]> expr
let extrude (x @ quote_local) = try raise (Extrusion x) with Extrusion x -> x
[%%expect{|
exception Extrusion of <[int]> expr
Line 2, characters 53-54:
2 | let extrude (x @ quote_local) = try raise (Extrusion x) with Extrusion x -> x
                                                         ^
Error: This value is "quote_local"
       but is expected to be "quote_global"
         because it is contained (via constructor "Extrusion") in the value at line 2, characters 42-55
         which is expected to be "quote_global".
|}];;

(* Lower-stage local captures are <[local]> *)

(* error: <[x]> is quote_local *)
let e = <[ fun (x @ local) -> $((<[ x ]> :@ quote_global)) ]>
[%%expect{|
Line 1, characters 36-37:
1 | let e = <[ fun (x @ local) -> $((<[ x ]> :@ quote_global)) ]>
                                        ^
Error: The value "x" is "local" to the parent region
       but is expected to be "global"
         because it is used inside the quoted expression at line 1, characters 33-40
         which is expected to be "global".
|}];;

(* infers that [g] accepts a <[local]> *)
let f g = <[ fun (x @ local) -> $(g <[ x ]>) ]>
[%%expect{|
Line 1, characters 39-40:
1 | let f g = <[ fun (x @ local) -> $(g <[ x ]>) ]>
                                           ^
Error: The value "x" is "local" to the parent region
       but is expected to be "global"
         because it is used inside the quoted expression at line 1, characters 36-43
         which is expected to be "global".
|}];;

(* Higher-stage <[local]> captures are local *)

(* error: <[x]> is quote_local *)
let f (x @ quote_local) = <[ fun () -> $x ]>
[%%expect{|
val f : 'a expr @ quote_local -> <[unit -> $('a)]> expr @ quote_local = <fun>
|}];;

(* Splice captures cross quoted modes like <[local]> *)

(* <[local]> crossed, only [g] is captured by the quote *)
let f (g : unit -> 'a expr @ quote_local) = <[ $(g ()) ]>
[%%expect{|
val f :
  ('a : any). (unit -> 'a expr @ quote_local) -> 'a expr @ quote_local once =
  <fun>
|}];;

(* [x] is captured by the quote and its mode is taken *)
let f (x @ quote_local) = <[ $x ]>
[%%expect{|
val f : ('a : any). 'a expr @ quote_local -> 'a expr @ quote_local once =
  <fun>
|}];;

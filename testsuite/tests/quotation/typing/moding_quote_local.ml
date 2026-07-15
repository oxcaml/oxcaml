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
val set : 'a ref @ quote_local -> 'a @ quote_local -> unit @ quote_local =
  <fun>
|}];;

exception Extrusion of <[int]> expr
let extrude (x @ quote_local) = try raise (Extrusion x) with Extrusion x -> x
[%%expect{|
exception Extrusion of <[int]> expr
val extrude : <[int]> expr @ quote_local -> <[int]> expr @ quote_local =
  <fun>
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
Uncaught exception: Failure("CR quoted-modes jbachurski: Locality_full; Locality_splice_and_full")

|}];;

(* Higher-stage <[local]> captures are local *)

(* error: <[x]> is quote_local *)
let f (x @ quote_local) = <[ fun () -> $x ]>
[%%expect{|
Uncaught exception: Failure("CR quoted-modes jbachurski: Locality_full; Locality_splice_and_full")

|}];;

(* Splice captures cross quoted modes like <[local]> *)

(* <[local]> crossed, only [g] is captured by the quote *)
let f (g : unit -> 'a expr @ quote_local) = <[ $(g ()) ]>
[%%expect{|
Uncaught exception: Failure("CR quoted-modes jbachurski: Locality_full; Locality_splice_and_full")

|}];;

(* [x] is captured by the quote and its mode is taken *)
let f (x @ quote_local) = <[ $x ]>
[%%expect{|
Uncaught exception: Failure("CR quoted-modes jbachurski: Locality_full; Locality_splice_and_full")

|}];;

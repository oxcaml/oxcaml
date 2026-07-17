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

let ignore = <[ fun (_ @ local) -> () ]>
[%%expect{|
val ignore : <[$('a) @ local -> unit]> expr = <[fun (_ : _ @ local) -> ()]>
|}];;

(* error: <[x]> is quote_local *)
let e = <[ fun (x @ local) -> $((<[ $ignore x ]> :@ quote_global)) ]>
[%%expect{|
Line 1, characters 44-45:
1 | let e = <[ fun (x @ local) -> $((<[ $ignore x ]> :@ quote_global)) ]>
                                                ^
Error: The value "x" is "quote_regional"
       but is expected to be "quote_global"
         because it is used inside the quoted expression at line 1, characters 33-48
         which is expected to be "quote_global"
         because it is used inside the quoted expression at line 1, characters 8-69
         which is expected to be "quote_global".
|}];;

(* infers that [g] accepts a <[local]> *)
let f g = <[ fun (x @ local) -> $(g <[ $ignore x ]>) ]>
[%%expect{|
val f :
  (<[unit]> expr @ quote_regional once -> 'a expr) ->
  <[$('b) @ local -> $('a)]> expr = <fun>
|}];;

(* Higher-stage <[local]> captures are local *)

(* <[x]> is quote_local, so the quoted closure is local *)
let f (x @ quote_local) = <[ fun () -> $x ]>
[%%expect{|
Line 1, characters 40-41:
1 | let f (x @ quote_local) = <[ fun () -> $x ]>
                                            ^
Error: The value "x" is "local"
       but is expected to be "global"
         because it is used inside the function at line 1, characters 29-41
         which is expected to be "global"
         because it is a quoted expression's result and thus always at the legacy modes.
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

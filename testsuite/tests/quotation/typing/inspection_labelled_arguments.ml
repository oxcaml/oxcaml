(* TEST
 modules = "util.ml inspection_labelled_arguments_types.ml";
 flags = "-extension runtime_metaprogramming";
 native;
*)

#syntax quotations on

(* In this test file we verify that the typed-tree representation in quotes has any
   disambiguations for labelled argument applications applied within it *)

open Util
open Inspection_labelled_arguments_types

(* For function applications, there are four cases of inspection:
   1. Commutativity (labelled, optional)
   2. Always-provided optional
   3. Eliminating omission (optional)
   4. Re-ordering omission (optional, labelled) *)

(* Commutativity *)
let (e1 : <[_ t1]> expr) = <[ fun f x y -> f ~x ~y ]>
let e1' = <[ let f = $e1 in ignore (f : _ t1) ]>
let () = test e1'

(* Always-provided optional *)
let (e21 : <[_ t21]> expr) = <[ fun f x -> f ~x () ]>
let e21' = <[ let f = $e21 in ignore (f : _ t21) ]>
let () = test e21'

let (e22 : <[_ t22]> expr) = <[ fun f x -> f ?x:(Some x) () ]>
let e22' = <[ let f = $e22 in ignore (f : _ t22) ]>
let () = test e22'

(* Eliminating omission *)
let (e3 : <[_ t3]> expr) = <[ fun f -> f () ]>
let e3' = <[ let f = $e3 in ignore (f : _ t3) ]>
let () = test e3'

(* Re-ordering omission *)
let (e4 : <[_ t4]> expr) = <[ fun f x -> f () ~x ]>
let e4' = <[ let f = $e4 in ignore (f : _ t4) ]>
let () = test e4'

(* Labelled tuples - only inspected for commutativity *)
let (et : <[_ tt]> expr) = <[ fun t -> let ~x, .. = t in x]>
let et' = <[ let f = $et in ignore (f : _ tt) ]>
let () = test et'

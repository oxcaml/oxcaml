(* TEST
 modules = "labeled_args_types.ml util.ml";
 flags = "-extension runtime_metaprogramming";
 arch_amd64;
 native;
*)

(* These tests verify that the typed-tree representation in quotes
   normalises labeled argument application *)

#syntax quotations on

open Util
open Labeled_args_types

(* For function applications, there are four cases of inspection,
   with some only applicable to optional/required arguments:
   1. Commutativity (optional, required)
   2. Always-provided (optional)
   3. Eliminating omission (optional)
   4. Re-ordering omission (optional, required) *)

(* Commutativity *)
let (e1_xy : <[_ t1]> expr) = <[ fun f x y -> f ~x ~y ]>
let e1_xy' = <[ let f = $e1_xy in ignore (f : _ t1) ]>
let (e1_yx : <[_ t1]> expr) = <[ fun f x y -> f ~y ~x ]>
let e1_yx' = <[ let f = $e1_yx in ignore (f : _ t1) ]>
let () = test e1_xy'; test e1_yx'

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
let (e4_pre : <[_ t4]> expr) = <[ fun f x -> f ~x () ]>
let e4_pre' = <[ let f = $e4_pre in ignore (f : _ t4) ]>
let (e4_post : <[_ t4]> expr) = <[ fun f x -> f () ~x ]>
let e4_post' = <[ let f = $e4_post in ignore (f : _ t4) ]>
let () = test e4_pre'; test e4_post'

(* Labeled tuples - only inspected for commutativity *)
let (et_x : <[_ tt]> expr) = <[ fun t -> let ~x, .. = t in x]>
let et_x' = <[ let f = $et_x in ignore (f : _ tt) ]>
let (et_y : <[_ tt]> expr) = <[ fun t -> let ~x, .. = t in x]>
let et_y' = <[ let f = $et_y in ignore (f : _ tt) ]>
let () = test et_x'; test et_y'

(* TEST
 include eval;
 modules = "labeled_args_types.ml util.ml";
 flags = "-extension runtime_metaprogramming";
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
let () =
   let (e1_xy : <[_ t1]> expr) = <[ fun f x y -> f ~x ~y ]> in
   let e1_xy' = <[ let f = $e1_xy in ignore (f : _ t1) ]> in
   let (e1_yx : <[_ t1]> expr) = <[ fun f x y -> f ~y ~x ]> in
   let e1_yx' = <[ let f = $e1_yx in ignore (f : _ t1) ]> in
   test e1_xy'; test e1_yx'

(* Always-provided optional *)
let () =
   let (e21 : <[_ t21]> expr) = <[ fun f x -> f ~x () ]> in
   let e21' = <[ let f = $e21 in ignore (f : _ t21) ]> in
   test e21'
let () =
   let (e22 : <[_ t22]> expr) = <[ fun f x -> f ?x:(Some x) () ]> in
   let e22' = <[ let f = $e22 in ignore (f : _ t22) ]> in
   test e22'

(* Eliminating omission *)
let () =
   let (e3 : <[_ t3]> expr) = <[ fun f -> f () ]> in
   let e3' = <[ let f = $e3 in ignore (f : _ t3) ]> in
   test e3'

(* Re-ordering omission *)
let () =
   let (e4_pre : <[_ t4]> expr) = <[ fun f x -> f ~x () ]> in
   let e4_pre' = <[ let f = $e4_pre in ignore (f : _ t4) ]> in
   let (e4_post : <[_ t4]> expr) = <[ fun f x -> f () ~x ]> in
   let e4_post' = <[ let f = $e4_post in ignore (f : _ t4) ]> in
   test e4_pre'; test e4_post'

(* Labeled tuples - only inspected for commutativity *)
let () =
   let (et_x : <[_ tt]> expr) = <[ fun t -> let ~x, .. = t in x]> in
   let et_x' = <[ let f = $et_x in ignore (f : _ tt) ]> in
   let (et_y : <[_ tt]> expr) = <[ fun t -> let ~x, .. = t in x]> in
   let et_y' = <[ let f = $et_y in ignore (f : _ tt) ]> in
   test et_x'; test et_y'

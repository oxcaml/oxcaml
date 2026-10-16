(* TEST
 flags = "-extension layout_poly_alpha";
 expect;
*)

(* Regression test for a bug where the poly instantiations would be generated in
   the wrong order so [use_id] couldn't see the code for [id]. *)

let (a, b) =
  let poly_ id x = x in
  let poly_ use_id x = id x in
  let a = use_id 1 in
  let b = use_id 2 in
  (a, b)

[%%expect{|
val a : int = 1
val b : int = 2
|}]

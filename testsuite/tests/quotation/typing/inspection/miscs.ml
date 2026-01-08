(* TEST
 modules = "util.ml";
 flags = "-extension runtime_metaprogramming";
 arch_amd64;
 native;
*)

#syntax quotations on

open Util

(* Propagating constraints on value bindings to pattern *)
let () = test <[
    let x = ("abc" : string @ local) in
    let _ : string @ local = x in ()
  ]>

(* Inferring format6 on a string *)
let () =
  let f (x : <[ _ format6 ]> expr) = x in
  test <[let p = $(f <[ "e_format6 says: %s %s\n\n" ]>) in Format.printf p "abc" "xyz" ]>

(* Eliminating optional arguments *)

(* FIXME: For ~eval:true, we need to prevent the eta expansion introduced by
   optional argument elimination from introducing a local-returning exclave *)
let () =
  let g = <[ fun ?(x = 0) y -> Format.printf "%d\n" (x + y) ]> in
  test ~eval:false <[ let f = $g in List.iter f [1; 2; 3] ]>

(* Module packing *)

(* FIXME: Support struct..end in quotes *)
(*
let () =
  let m = <[ let module M = struct let f x y = x + y end in (module M) ]> in
  test <[ let (module M) = $m in M.f 2 2 ]>
*)

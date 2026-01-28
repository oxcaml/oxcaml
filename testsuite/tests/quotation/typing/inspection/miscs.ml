(* TEST
 modules = "miscs_types.ml util.ml";
 flags = "-extension runtime_metaprogramming";
 arch_amd64;
 native;
*)

#syntax quotations on

open Util
open Miscs_types

(* Propagating constraints on value bindings to pattern *)
let () = test <[
    let x = ("abc" : string) in
    let _ : string = x in ()
  ]>

(* Inferring format6 on a string *)
(* FIXME: For ~eval:true, linking fails as some Format symbols are missing. *)
let () =
  let f (x : <[ _ format6 ]> expr) = x in
  test ~eval:false
    <[let p = $(f <[ "e_format6 says: %s %s\n\n" ]>)
      in Format.printf p "abc" "xyz" ]>

(* Eliminating optional arguments *)
(* FIXME: For ~eval:true, we need to prevent the eta expansion introduced by
   optional argument elimination from introducing a local-returning exclave *)
let () =
  let g = <[ fun ?(x = 0) y -> Format.printf "%d\n" (x + y) ]> in
  test ~eval:false
    <[ let f = $g in List.iter f [1; 2; 3] ]>

(* Module packing *)
(* FIXME: Ideally, we probably want module patterns to work,
   but they fail as only global module names can be used. *)
let () =
  let (e : <[unit -> (module S)]> expr) = <[fun () -> (module M)]> in
  test <[ ignore (((fun x -> x) $e) : unit -> (module S)) ]>

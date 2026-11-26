(* TEST
 modules = "external_code_a.ml external_code_b.ml";
 flambda2;
 {
   native;
 }{
   jsir;
 }{
   ocamlopt_flags = "-link-jsir";
   jsir;
 }{
   ocamlopt_flags = "-link-jsir -g";
   jsir;
 }
*)

(* When Flambda inlines a function of another unit, the copy of its body can
   create closures from code of that unit. Checks that such code is found:
   code that uses its closure is translated from its definition in the .cmx
   file of the other unit, and the closure of code that does not is fetched
   from the other unit (see the README of the JSIR translation). The arguments
   are opaque so that Flambda has no reason to specialise the code, which
   would make it code of this unit. *)

module A = External_code_a
module B = External_code_b

let () =
  let n = Sys.opaque_identity 10 in
  let add = Sys.opaque_identity (A.make_adder n) in
  let add' = Sys.opaque_identity (B.make_adder n) in
  Printf.printf "adder: %d %d %d\n" (add 1) (add' 1) (B.add3 1)

let () =
  let n = Sys.opaque_identity 10 in
  let count = Sys.opaque_identity (A.make_counter n) in
  let f = count () in
  let g = count () in
  Printf.printf "counter: %d %d %d\n" (f 1) (g 1) (A.get ())

let () =
  let n = Sys.opaque_identity 10 in
  let first = Sys.opaque_identity (A.make_first n) in
  let first' = Sys.opaque_identity (B.make_first n) in
  Printf.printf "first: %d %d\n" (first 1) (first' 2)

module String_max = A.Make (String)

module Opaque_max =
  A.Make ((val Sys.opaque_identity (module String : A.S with type t = string)))

let () =
  Printf.printf "functor: %d %d %s %s %d %s\n"
    (B.Int_max.max_list [1; 3; 2])
    (B.Opaque_max.max_list [1; 3; 2])
    (String_max.max_list ["a"; "c"; "b"])
    (Opaque_max.max_list ["a"; "c"; "b"])
    (B.Opaque_max.id 1) (Opaque_max.id "x")

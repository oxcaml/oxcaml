(* TEST
 include stdlib_upstream_compatible;
 reference = "${test_source_directory}/open.reference";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

open Stdlib_upstream_compatible
external [@layout_poly] id : ('a : any). 'a -> 'a = "%opaque"


let _ = print_endline "Test 1: Open with module ident"

module M = struct let foo = #42.0 let bar = "hello" end

let _ =
  let open M in
  print_float (Float_u.to_float (id foo));
  print_string " ";
  print_endline (id bar)


let _ = print_endline "Test 2: Open with inline struct"

let _ =
  let open struct let foo = #42.0 let bar = "hello" end in
  print_float (Float_u.to_float (id foo));
  print_string " ";
  print_endline (id bar)


let _ = print_endline "Test 3: Open with functor"

module Functor (X : sig end) = struct let foo = #42.0 let bar = "hello" end

let _ =
  let open Functor(struct end) in
  print_float (Float_u.to_float (id foo));
  print_string " ";
  print_endline (id bar)

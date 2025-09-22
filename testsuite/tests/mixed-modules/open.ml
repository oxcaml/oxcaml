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


let _ = print_endline "Test 1: [let open] with module ident"

module M = struct let foo = #42.0 let bar = "hello" end

let _ =
  let open M in
  print_float (Float_u.to_float (id foo));
  print_string " ";
  print_endline (id bar)


let _ = print_endline "Test 2: [let open] with inline struct"

let _ =
  let open struct let foo = #42.0 let bar = "hello" end in
  print_float (Float_u.to_float (id foo));
  print_string " ";
  print_endline (id bar)


let _ = print_endline "Test 3: [let open] with functor"

module Functor (X : sig end) = struct let foo = #42.0 let bar = "hello" end

let _ =
  let open Functor(struct end) in
  print_float (Float_u.to_float (id foo));
  print_string " ";
  print_endline (id bar)


let _ = print_endline "Test 4: Tests 1-3 with [open] instead of [let open]"

module M_4_1 = struct
  open M
  let _ = print_float (Float_u.to_float (id foo))
  let _ = print_string " "
  let _ = print_endline (id bar)
end

module M_4_2 = struct
  open struct let foo = #42.0 let bar = "hello" end
  let _ = print_float (Float_u.to_float (id foo))
  let _ = print_string " "
  let _ = print_endline (id bar)
end

module M_4_3 = struct
  open Functor(struct end)
  let _ = print_float (Float_u.to_float (id foo))
  let _ = print_string " "
  let _ = print_endline (id bar)
end

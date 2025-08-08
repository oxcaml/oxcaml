(* TEST
 include stdlib_upstream_compatible;
 reference = "${test_source_directory}/aliases.reference";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)


open Stdlib_upstream_compatible
external [@layout_poly] id : ('a : any). 'a -> 'a = "%opaque"


let _ = print_endline "Test 1: Based on pr11186.ml"

module M =
  (((struct
       module N = struct let x = #4.0 let s = "Hello" end
       module A = N
       module B = A
       module C = B
     end : sig
       module A : sig val x : float# val s : string end
       module B = A
       module C = B
     end) : sig
      module B : sig val x : float# val s : string end
      module C = B
    end) : sig
     module C : sig val x : float# val s : string end
   end)

let _ = print_float (Float_u.to_float (id M.C.x))
let _ = print_string " "
let _ = print_endline (id M.C.s)

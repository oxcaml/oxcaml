(* TEST
 include stdlib_upstream_compatible;
 reference = "${test_source_directory}/basics.reference";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

open Stdlib_upstream_compatible
external [@layout_poly] id : ('a : any). 'a -> 'a = "%opaque"

let _ = print_endline "Test 1: basic mixed module"

module My_module = struct
  let foo = "a"
  let bar = #5L
  let baz = "y"
  let qux = 10
end

let _ = print_int (Int64_u.to_int (id My_module.bar))
let _ = print_endline ""
let _ = print_int (id My_module.qux)
let _ = print_endline ""


let _ = print_endline "Test 2: shadowing within a module"

module Shadow_test = struct
  let foo = "a"
  let x = #10.0

  let _ = print_float (Float_u.to_float (id x))
  let _ = print_string " "

  let x = #20.0
  let baz = "y"

  let _ = print_float (Float_u.to_float (id x))
  let _ = print_endline ""
end

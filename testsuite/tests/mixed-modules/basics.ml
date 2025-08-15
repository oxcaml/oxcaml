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
external int_u_of_int : int -> int# = "%int#_of_int"
external int_of_int_u : int# -> int = "%int_of_int#"

let _ = print_endline "Test 1: basic mixed module"

module My_module = struct
  let foo = "a"
  let bar = #5l
  let baz = "y"
  let qux = 10
  let zil = int_u_of_int 25
end

let _ = print_int (Int32_u.to_int (id My_module.bar))
let _ = print_endline ""
let _ = print_int (id My_module.qux)
let _ = print_endline ""
let _ = print_int (int_of_int_u (id My_module.zil))
let _ = print_endline ""


let _ = print_endline "Test 2: shadowing within a module"

module Shadow = struct
  let foo = "a"
  let x = #10.0

  let i_1 = print_float (Float_u.to_float (id x)); 1
  let i_2 = print_string " "; 2

  let x = #20.0
  let baz = "y"

  let i_3 = print_float (Float_u.to_float (id x)); 3
  let i_4 = print_endline ""; 4
end


let _ = print_endline "Test 3: pattern aliases"

module Pat_alias = struct let #(a, b) as c = #("a", #1.0) end

let _ = print_string (id Pat_alias.a)
let _ = print_string " "
let _ = print_float (Float_u.to_float (id Pat_alias.b))
let _ = print_string " "
let _ =
  let #(x, y) = id Pat_alias.c in
  print_string x;
  print_string " ";
  print_float (Float_u.to_float y)
let _ = print_endline ""

type point = #{ x : float#; y : float# }
type labeled_point = #{ x : float#; y : float#; label : string }
type name = #{ first_name : string; last_name : string }

module Pat_alias_sig_check : sig
  val a : string
  val b : float#
  val c : #(string * float#)
end = Pat_alias

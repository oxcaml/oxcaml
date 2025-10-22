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

external id : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly]
external int_u_of_int : int -> int# = "%int#_of_int"
external int_of_int_u : int# -> int = "%int_of_int#"
type void : void
external void : unit -> void = "%unbox_unit"


let () = print_endline "Test: basic mixed module"

module My_module = struct
  let foo = "a"
  let bar = #5l
  let baz = "y"
  let qux = 10
  let zil = int_u_of_int 25
end

let () =
  print_endline "Expected: 5 10 25";
  Printf.printf
    "Actual:   %d %d %d\n\n"
    (Int32_u.to_int (id My_module.bar))
    (id My_module.qux)
    (int_of_int_u (id My_module.zil))


let () = print_endline "Test: shadowing within a module"

module Shadow = struct
  let foo = "a"
  let x = #10.0

  let i_1 = Printf.printf "%.1f " (Float_u.to_float (id x)); 1

  let x = #20.0
  let baz = "y"

  let i_2 = Printf.printf "%.1f " (Float_u.to_float (id x)); 2

  let x = "30.0"

  let i_3 = Printf.printf "%s\n\n" (id x); 3
end


let () = print_endline "Test: pattern aliases"

module Pat_alias = struct
  let (#(a, b) as c) = #("a", #1.0)
end

let () =
  let #(x, y) = id Pat_alias.c in
  print_endline "Expected: a 1.0 a 1.0";
  Printf.printf
    "Actual:   %s %.1f %s %.1f\n\n"
    (id Pat_alias.a)
    (Float_u.to_float (id Pat_alias.b))
    (id x)
    (Float_u.to_float y)

type point =
  #{ x : float#
   ; y : float#
   }

type labeled_point =
  #{ x : float#
   ; y : float#
   ; label : string
   }

type name =
  #{ first_name : string
   ; last_name : string
   }

module Pat_alias_sig_check : sig
  val a : string
  val b : float#
  val c : #(string * float#)
end = Pat_alias


let () = print_endline "Test: complicated unboxed products"

module Complicated_unboxed_products = struct
  let foo = #("0", #1l, "2")
  let bar = #(#3.0, "4", void (), #5.0)
  let baz = #(#6l, void (), #(7.0, #8.0, "9", void (), "10"), "11")
  let vaz = void ()
  let qux = "12"
  let zil = #13L
end

let () =
  let #(a, b, c) = id Complicated_unboxed_products.foo in
  let #(d, e, _void, f) = id Complicated_unboxed_products.bar in
  let #(g, _void, #(h, i, j, _void_2, k), l) =
    id Complicated_unboxed_products.baz
  in
  let m = id Complicated_unboxed_products.qux in
  let n = id Complicated_unboxed_products.zil in
  Printf.printf
    "%s %d %s %.0f %s %.0f %d %.0f %.0f %s %s %s %s %d\n"
    a
    (Int32_u.to_int b)
    c
    (Float_u.to_float d)
    e
    (Float_u.to_float f)
    (Int32_u.to_int g)
    h
    (Float_u.to_float i)
    j
    k
    l
    m
    (Int64_u.to_int n)

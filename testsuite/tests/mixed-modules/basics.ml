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
;;

let () = print_endline "Test: shadowing within a module"

module Shadow = struct
  let foo = "a"
  let x = #10.0

  let i_1 =
    Printf.printf "%.1f " (Float_u.to_float (id x));
    1
  ;;

  let x = #20.0
  let baz = "y"

  let i_2 =
    Printf.printf "%.1f " (Float_u.to_float (id x));
    2
  ;;

  let x = "30.0"

  let i_3 =
    Printf.printf "%s\n\n" (id x);
    3
  ;;
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
;;

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
end =
  Pat_alias

let () = print_endline "Test: complicated unboxed products"

module Complicated_unboxed_products = struct
  let foo = #("0", #1l, "2")
  let bar = #(#3.0, "4", #5.0)
  let baz = #(#6l, #(7.0, #8.0, "9", "10"), "11")
  let qux = "12"
  let zil = #13L
end

let () =
  let #(a, b, c) = id Complicated_unboxed_products.foo in
  let #(d, e, f) = id Complicated_unboxed_products.bar in
  let #(g, #(h, i, j, k), l) = id Complicated_unboxed_products.baz in
  let m = id Complicated_unboxed_products.qux in
  let n = id Complicated_unboxed_products.zil in
  Printf.printf
    "%s %d %s %.1f %s %.1f %d %.1f %.1f %s %s %s %s %d\n\n"
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
;;

let () = print_endline "Test: simple mixed products"

module Simple_mixed_products = struct
  let mixed_product_1 = #("hello", #42L)
  let mixed_product_2 = #(#3.14, "world")
  let all_unboxed = #(#1.5, #100L)
  let three_elements = #("a", #2l, "b")
end

let () =
  let #(a, b) = id Simple_mixed_products.mixed_product_1 in
  let #(c, d) = id Simple_mixed_products.mixed_product_2 in
  let #(e, f) = id Simple_mixed_products.all_unboxed in
  let #(g, h, i) = id Simple_mixed_products.three_elements in
  print_endline "Expected: hello 42 3.14 world 1.5 100 a 2 b";
  Printf.printf
    "Actual:   %s %d %.2f %s %.1f %d %s %d %s\n\n"
    a
    (Int64_u.to_int b)
    (Float_u.to_float c)
    d
    (Float_u.to_float e)
    (Int64_u.to_int f)
    g
    (Int32_u.to_int h)
    i
;;

let () = print_endline "Test: deeply nested mixed products"

module Nested_mixed_products = struct
  let level_1 = #(#1.0, #("inner", #2L), "outer")
  let level_2 = #(#(#3.0, "a"), #(#4L, #("b", #5l)), "c")
  let regular_int = 99
end

let () =
  let #(a, #(b, c), d) = id Nested_mixed_products.level_1 in
  let #(#(e, f), #(g, #(h, i)), j) = id Nested_mixed_products.level_2 in
  print_endline "Expected: 1.0 inner 2 outer 3.0 a 4 b 5 c 99";
  Printf.printf
    "Actual:   %.1f %s %d %s %.1f %s %d %s %d %s %d\n\n"
    (Float_u.to_float a)
    b
    (Int64_u.to_int c)
    d
    (Float_u.to_float e)
    f
    (Int64_u.to_int g)
    h
    (Int32_u.to_int i)
    j
    (id Nested_mixed_products.regular_int)
;;

let () = print_endline "Test: mixed products with different unboxed types"

module Different_unboxed_types = struct
  let with_int32 = #(#10l, "ten", #20L)
  let with_float = #(#1.5, #2l, #3L)
  let with_int = #(int_u_of_int 100, "hundred", #200.0)
  let regular_value = "regular"
end

let () =
  let #(a, b, c) = id Different_unboxed_types.with_int32 in
  let #(d, e, f) = id Different_unboxed_types.with_float in
  let #(g, h, i) = id Different_unboxed_types.with_int in
  print_endline "Expected: 10 ten 20 1.5 2 3 100 hundred 200.0 regular";
  Printf.printf
    "Actual:   %d %s %d %.1f %d %d %d %s %.1f %s\n\n"
    (Int32_u.to_int a)
    b
    (Int64_u.to_int c)
    (Float_u.to_float d)
    (Int32_u.to_int e)
    (Int64_u.to_int f)
    (int_of_int_u g)
    h
    (Float_u.to_float i)
    (id Different_unboxed_types.regular_value)
;;

let () = print_endline "Test: void in mixed module"

type void : void

external void : unit -> void = "%unbox_unit"

module With_void = struct
  let regular_before = "before"
  let void_value = void ()
  let unboxed_float = #42.0
  let regular_after = "after"
  let unboxed_int64 = #99L
end

let () =
  print_endline "Expected: before 42.0 after 99";
  Printf.printf
    "Actual:   %s %.1f %s %d\n\n"
    (id With_void.regular_before)
    (Float_u.to_float (id With_void.unboxed_float))
    (id With_void.regular_after)
    (Int64_u.to_int (id With_void.unboxed_int64))
;;

let () = print_endline "Test: void with mixed products"

module Void_with_products = struct
  let product_before_void = #("a", #1.0)
  let void_val = void ()
  let product_after_void = #(#2L, "b", #3l)
  let regular = 100
end

let () =
  let #(a, b) = id Void_with_products.product_before_void in
  let #(c, d, e) = id Void_with_products.product_after_void in
  print_endline "Expected: a 1.0 2 b 3 100";
  Printf.printf
    "Actual:   %s %.1f %d %s %d %d\n\n"
    a
    (Float_u.to_float b)
    (Int64_u.to_int c)
    d
    (Int32_u.to_int e)
    (id Void_with_products.regular)
;;

let () = print_endline "Test: void inside unboxed products"

module Void_in_products = struct
  let simple = #(#1.0, void (), #2L)
  let with_string = #("hello", void (), #42.0)
  let nested = #("outer", #(#3.0, void (), "inner"), #4l)
  let multiple_voids = #(void (), #5L, void (), "middle", void (), #6.0, void ())
end

let () =
  let #(a, _void1, b) = id Void_in_products.simple in
  let #(c, _void2, d) = id Void_in_products.with_string in
  let #(e, #(f, _void3, g), h) = id Void_in_products.nested in
  let #(_v1, i, _v2, j, _v3, k, _v4) = id Void_in_products.multiple_voids in
  print_endline "Expected: 1.0 2 hello 42.0 outer 3.0 inner 4 5 middle 6.0";
  Printf.printf
    "Actual:   %.1f %d %s %.1f %s %.1f %s %d %d %s %.1f\n"
    (Float_u.to_float a)
    (Int64_u.to_int b)
    c
    (Float_u.to_float d)
    e
    (Float_u.to_float f)
    g
    (Int32_u.to_int h)
    (Int64_u.to_int i)
    j
    (Float_u.to_float k)
;;

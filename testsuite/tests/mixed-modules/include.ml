(* TEST
 include stdlib_upstream_compatible;
 reference = "${test_source_directory}/include.reference";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

open Stdlib_upstream_compatible

external id : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly]
type void : void
external void : unit -> void = "%unbox_unit"


(* Tests exercising errors are in `typing.ml` *)

let _ = print_endline "Test: include with module ident"

module M = struct
  let foo = #42.0
  let bar = "hello"
  let product = #(#1.0, void (), #100L, "test")
end

module M1 = struct
  let pre_val = "before"
  let pre_unboxed = #99L
  let pre_product = #(void (), #2.5, "pre")

  include M

  let post_val = "after"
  let post_unboxed = #3.14
  let post_product = #(#7L, void (), "post")

  let () =
    let #(a, _v1, b, c) = id product in
    let #(_v2, d, e) = id pre_product in
    let #(f, _v3, g) = id post_product in
    print_endline "Expected: before 99 2.5 pre 42.0 hello 1.0 100 test after 3.14 7 post";
    Printf.printf "Actual:   %s %d %.1f %s %.1f %s %.1f %d %s %s %.2f %d %s\n\n"
      (id pre_val)
      (Int64_u.to_int (id pre_unboxed))
      (Float_u.to_float d)
      e
      (Float_u.to_float (id foo))
      (id bar)
      (Float_u.to_float a)
      (Int64_u.to_int b)
      c
      (id post_val)
      (Float_u.to_float (id post_unboxed))
      (Int64_u.to_int f)
      g
  ;;
end


let _ = print_endline "Test: include with inline struct"

module M2 = struct
  include struct
    let foo = #42.0
    let bar = "hello"
    let product = #(void (), #2L, "inline", #3.0)
  end
  let qux = #3.14

  let () =
    let #(_v, a, b, c) = id product in
    print_endline "Expected: 42.0 hello 2 inline 3.0 3.14";
    Printf.printf
      "Actual:   %.1f %s %d %s %.1f %.2f\n\n"
      (Float_u.to_float (id foo))
      (id bar)
      (Int64_u.to_int a)
      b
      (Float_u.to_float c)
      (Float_u.to_float (id qux))
  ;;
end


let () = print_endline "Test: include with functor"

module Functor (X : sig end) = struct
  let foo = #42.0
  let bar = "hello"
  let product = #(#4.0, "functor", void (), #5L)
end

module M3 = struct
  include Functor (struct end)

  let qux = #3.14

  let () =
    let #(a, b, _v, c) = id product in
    print_endline "Expected: 42.0 hello 4.0 functor 5 3.14";
    Printf.printf
      "Actual:   %.1f %s %.1f %s %d %.2f\n\n"
      (Float_u.to_float (id foo))
      (id bar)
      (Float_u.to_float a)
      b
      (Int64_u.to_int c)
      (Float_u.to_float (id qux))
  ;;
end


let () = print_endline "Test: multiple includes"

module A = struct
  let a = #1.0
  let b = "from A"
  let product_a = #(#6L, void (), "A")
end

module B = struct
  let c = #2.0
  let d = "from B"
  let product_b = #(void (), #7.0, "B")
end

module M4 = struct
  include A
  include B

  let e = #3.0

  let () =
    let #(f, _v1, g) = id product_a in
    let #(_v2, h, i) = id product_b in
    print_endline "Expected: 1.0 from A 2.0 from B 3.0 6 A 7.0 B";
    Printf.printf
      "Actual:   %.1f %s %.1f %s %.1f %d %s %.1f %s\n\n"
      (Float_u.to_float (id a))
      (id b)
      (Float_u.to_float (id c))
      (id d)
      (Float_u.to_float (id e))
      (Int64_u.to_int f)
      g
      (Float_u.to_float h)
      i
  ;;
end


let () = print_endline "Test: include shadowing include"

module Base = struct
  let x = #10.0
  let y = "original"
  let z = 100
  let product = #(#8L, "base", void ())
end

module Override = struct
  let x = #20.0
  let y = "overridden"
  let product = #(void (), #9L, "override")
end

module M5 = struct
  include Base
  include Override

  let () =
    let #(_v, a, b) = id product in
    print_endline "Expected: 20.0 overridden 100 9 override";
    Printf.printf
      "Actual:   %.1f %s %d %d %s\n\n"
      (Float_u.to_float (id x))
      (id y)
      (id z)
      (Int64_u.to_int a)
      b
  ;;
end


let () = print_endline "Test: include shadowing a val"

module M6 = struct
  let a = #5.0
  let b = "before_include"
  let product_1 = #(void (), #10L, "before")

  include struct
    let b = "from_include"
    let c = #7.0
    let product_1 = #(#11L, void (), "from_include")
  end

  let () =
    let #(d, _v, e) = id product_1 in
    print_endline "Expected: 5.0 from_include 7.0 11 from_include";
    Printf.printf
      "Actual:   %.1f %s %.1f %d %s\n\n"
      (Float_u.to_float (id a))
      (id b)
      (Float_u.to_float (id c))
      (Int64_u.to_int d)
      e
  ;;
end


let () = print_endline "Test: val shadowing an include"

module M7 = struct
  include struct
    let p = #0L
    let q = "from_include"
    let product = #(#12L, "from_include", void ())
  end

  let p = #9.0
  let r = "after_include"
  let product = #(void (), #13L, "shadowed")

  let () =
    let #(_v, a, b) = id product in
    print_endline "Expected: 9.0 from_include after_include 13 shadowed";
    Printf.printf
      "Actual:   %.1f %s %s %d %s\n"
      (Float_u.to_float (id p))
      (id q)
      (id r)
      (Int64_u.to_int a)
      b
  ;;
end

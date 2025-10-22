(* TEST
 include stdlib_upstream_compatible;
 reference = "${test_source_directory}/coercions.reference";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

open Stdlib_upstream_compatible

external [@layout_poly] id : ('a : any). 'a -> 'a = "%opaque"
type void : void
external void : unit -> void = "%unbox_unit"


let () = print_endline "Test: mixed to mixed"

module Module_1 = struct
  let foo = "hello"
  let bar = #5L
  let baz = "world"
  let qux = 10
  let zil = #15.0
end

module Coerced_module_1 : sig
  val bar : int64#
  val qux : int
end =
  Module_1

let () =
  print_endline "Expected: 5 10";
  Printf.printf
    "Actual:   %d %d\n\n"
    (Int64_u.to_int (id Coerced_module_1.bar))
    (id Coerced_module_1.qux)


let () = print_endline "Test: mixed to value only"

module Coerced_module_2 : sig
  val qux : int
  val foo : string
end =
  Module_1

let () =
  print_endline "Expected: 10 hello";
  Printf.printf "Actual:   %d %s\n\n" (id Coerced_module_2.qux) (id Coerced_module_2.foo)

n
let () = print_endline "Test: nested modules"

module Module_3 = struct
  module Numbers = struct
    let smallest_float_u = #0.0

    module Zero = struct
      let as_float_u = #0.0
      let as_float = 0.0
      let as_string = "0"
      let as_int64_u = #0L
    end

    module One = struct
      let as_string = "1"
      let as_float = 1.0
    end

    module Two = struct
      let as_float = 2.0
      let as_string = "2"
      let as_int64_u = #2L
      let as_float_u = #2.0
    end

    let biggest_float_u = #2.0
  end
end

module Coerced_module_3 : sig
  module Numbers : sig
    module One : sig
      val as_float : float
    end

    module Zero : sig
      val as_float_u : float#
      val as_float : float
    end

    val smallest_float_u : float#
  end
end =
  Module_3

let () =
  print_endline "Expected: 1.0 0.0 0.0 0.0";
  Printf.printf
    "Actual:   %.1f %.1f %.1f %.1f\n\n"
    (id Coerced_module_3.Numbers.One.as_float)
    (id Coerced_module_3.Numbers.Zero.as_float)
    (Float_u.to_float (id Coerced_module_3.Numbers.Zero.as_float_u))
    (Float_u.to_float (id Coerced_module_3.Numbers.smallest_float_u))


let () = print_endline "Test: composed coercions"

module type S = sig
  module K : sig
    val x : float#
    val t : string
    val y : float#
    val s : string
  end
end

module type S' = sig
  module K : sig
    val y : float#
    val s : string
    val x : float#
  end
end

module M = struct
  module K : sig
    val s : string
    val y : float#
    val x : float#
    val t : string
  end = struct
    let x = #1.0
    let y = #2.0
    let s = "s"
    let t = "s"
    let z = #3.0
  end
end

module N : S' = (M : S)

let () =
  print_endline "Expected: 1.0 2.0 s";
  Printf.printf
    "Actual:   %.1f %.1f %s\n\n"
    (Float_u.to_float (id N.K.x))
    (Float_u.to_float (id N.K.y))
    (id N.K.s)


let () = print_endline "Test: coercing modules with mixed products"

module With_products = struct
  let simple = #(#1.0, "one")
  let nested = #(#2L, #("two", #3.0))
  let x = 100
  let another_unboxed = #4l
end

module Coerced_products : sig
  val simple : #(float# * string)
  val x : int
end =
  With_products

let () =
  let #(f, s) = id Coerced_products.simple in
  print_endline "Expected: 1.0 one 100";
  Printf.printf
    "Actual:   %.1f %s %d\n\n"
    (Float_u.to_float f)
    s
    (id Coerced_products.x)


let () = print_endline "Test: coercing modules with nested mixed products"

module Complex_products = struct
  let level1 = #(#10.0, #("a", #20L))
  let level2 = #(#(#30L, "b"), #40.0)
  let boxed = "boxed"
end

module Coerced_complex : sig
  val level1 : #(float# * #(string * int64#))
  val boxed : string
end =
  Complex_products

let () =
  let #(f, #(s, i)) = id Coerced_complex.level1 in
  print_endline "Expected: 10.0 a 20 boxed";
  Printf.printf
    "Actual:   %.1f %s %d %s\n\n"
    (Float_u.to_float f)
    s
    (Int64_u.to_int i)
    (id Coerced_complex.boxed)


let () = print_endline "Test: coercing modules with void"

module With_void = struct
  let void_val = void ()
  let float_val = #5.0
  let string_val = "hello"
  let another_float = #10.0
end

module Coerced_void : sig
  val float_val : float#
  val void_val : void
  val string_val : string
end =
  With_void

let () =
  print_endline "Expected: hello 5.0";
  Printf.printf
    "Actual:   %s %.1f\n"
    (id Coerced_void.string_val)
    (Float_u.to_float (id Coerced_void.float_val))

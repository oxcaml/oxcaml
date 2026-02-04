(* TEST
 include stdlib_upstream_compatible;
 reference = "${test_source_directory}/functors.reference";
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


let () = print_endline "Test: no coercion in or out"

module type Number = sig
  val as_float_u : float#
  val as_string : string
end

module Incr (M : Number) : Number = struct
  let as_float_u = Float_u.add M.as_float_u #1.0
  let as_string = M.as_string ^ "+1"
end

module One = struct
  let as_float_u = #1.0
  let as_string = "1"
end

module Two = Incr (One)

let () =
  print_endline "Expected: 2.0 1+1";
  Printf.printf
    "Actual:   %.1f %s\n\n"
    (Float_u.to_float (id Two.as_float_u))
    (id Two.as_string)


let () = print_endline "Test: coercion in, no coercion out"

module Ten = struct
  let as_int = 10
  let as_float_u = #10.0
  let as_string = "10"
  let as_int64_u = #10L
end

module Eleven = Incr (Ten)

let () =
  print_endline "Expected: 11.0 10+1";
  Printf.printf
    "Actual:   %.1f %s\n\n"
    (Float_u.to_float (id Eleven.as_float_u))
    (id Eleven.as_string)


let () = print_endline "Test: coercion out, no coercion in"

module Double (M : Number) : Number = struct
  let undoubled_float = M.as_float_u
  let as_string = "(" ^ M.as_string ^ ")*2"
  let as_float_u = Float_u.add undoubled_float undoubled_float
end

module Four = Double (Two)

let () =
  print_endline "Expected: 4.0 (1+1)*2";
  Printf.printf
    "Actual:   %.1f %s\n\n"
    (Float_u.to_float (id Four.as_float_u))
    (id Four.as_string)


let () = print_endline "Test: coercion in and out"

module Three = struct
  let as_int = 3
  let as_float_u = #3.0
  let as_string = "3"
  let as_int64_u = #3L
end

module Six = Double (Three)

let () =
  print_endline "Expected: 6.0 (3)*2";
  Printf.printf
    "Actual:   %.1f %s\n\n"
    (Float_u.to_float (id Six.as_float_u))
    (id Six.as_string)


let () = print_endline "Test: generative functor"

module type Counting_sig = sig
  val boxed_one : float
  val unboxed_one : float#
  val boxed_two : float
  val unboxed_two : float#
end

module MakeCounting () : Counting_sig = struct
  let boxed_one = 1.0
  let unboxed_one = #1.0
  let boxed_two = 2.0
  let unboxed_two = #2.0
  let boxed_three = 3.0
  let unboxed_three = #3.0
end

module Counting = MakeCounting ()

let () =
  print_endline "Expected: 1.0 2.0";
  Printf.printf
    "Actual:   %.1f %.1f\n\n"
    (Float_u.to_float (id Counting.unboxed_one))
    (Float_u.to_float (id Counting.unboxed_two))


let () = print_endline "Test: functor with mixed products"

module type With_products = sig
  val simple_product : #(float# * string)
  val nested_product : #(int64# * #(string * float# * string))
end

module Add_to_products (M : With_products) : With_products = struct
  let simple_product =
    let #(f, s) = M.simple_product in
    #(Float_u.add f #10.0, s ^ "+10")

  let nested_product =
    let #(i, #(s, f, s2)) = M.nested_product in
    #(Int64_u.add i #5L, #(s ^ "+5", Float_u.add f #5.0, s2 ^ "+5"))
end

module Base_products = struct
  let simple_product = #(#1.5, "1.5")
  let nested_product = #(#10L, #("ten", #20.0, "twenty"))
end

module Augmented = Add_to_products (Base_products)

let () =
  let #(f, s) = id Augmented.simple_product in
  let #(i, #(s2, f2, s3)) = id Augmented.nested_product in
  print_endline "Expected: 11.5 1.5+10 15 ten+5 25.0 twenty+5";
  Printf.printf
    "Actual:   %.1f %s %d %s %.1f %s\n\n"
    (Float_u.to_float f)
    s
    (Int64_u.to_int i)
    s2
    (Float_u.to_float f2)
    s3


let () = print_endline "Test: functor with void"

module type With_void = sig
  val void_val : void
  val float_val : float#
  val string_val : string
end

module Transform_with_void (M : With_void) : With_void = struct
  let void_val = M.void_val
  let float_val = Float_u.mul M.float_val #2.0
  let string_val = M.string_val ^ "*2"
end

module Void_base = struct
  let string_val = "three"
  let void_val = void ()
  let float_val = #3.0
end

module Void_transformed = Transform_with_void (Void_base)

let () =
  print_endline "Expected: 6.0 three*2";
  Printf.printf
    "Actual:   %.1f %s\n"
    (Float_u.to_float (id Void_transformed.float_val))
    (id Void_transformed.string_val)

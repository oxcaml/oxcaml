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
external [@layout_poly] id : ('a : any). 'a -> 'a = "%opaque"

let _ = print_endline "Test: no coercion in or out"

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

let _ = print_float (Float_u.to_float (id Two.as_float_u))
let _ = print_endline ""
let _ = print_endline (id Two.as_string)


let _ = print_endline "Test: coercion in, no coercion out"

module Ten = struct
  let as_int = 10
  let as_float_u = #10.0
  let as_string = "10"
  let as_int64_u = #10L
end

module Eleven = Incr (Ten)

let _ = print_float (Float_u.to_float (id Eleven.as_float_u))
let _ = print_endline ""
let _ = print_endline (id Eleven.as_string)


let _ = print_endline "Test: coercion out, no coercion in"

module Double (M : Number) : Number = struct
  let undoubled_float = M.as_float_u
  let as_string = "(" ^ M.as_string ^ ")*2"
  let as_float_u = Float_u.add undoubled_float undoubled_float
end

module Four = Double(Two)

let _ = print_float (Float_u.to_float (id Four.as_float_u))
let _ = print_endline ""
let _ = print_endline (id Four.as_string)


let _ = print_endline "Test: coercion in and out"

module Three = struct
  let as_int = 3
  let as_float_u = #3.0
  let as_string = "3"
  let as_int64_u = #3L
end

module Six = Double (Three)

let _ = print_float (Float_u.to_float (id Six.as_float_u))
let _ = print_endline ""
let _ = print_endline (id Six.as_string)


let _ = print_endline "Test: generative functor"

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

let _ =
  print_float (Float_u.to_float (id Counting.unboxed_one));
  print_endline "";
  print_float (Float_u.to_float (id Counting.unboxed_two));
  print_endline ""

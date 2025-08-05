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


let _ = print_endline "Test 1: no coercion in or out"

module type Number = sig
  val as_float_u : float#
  val as_string : string
end

module Incr (M : Number) : Number = struct
  let as_float_u = Float_u.add M.as_float_u #1.0
  let as_string = M.as_string ^ "+1"
end

module Zero = struct
  let as_float_u = #0.0
  let as_string = "0"
end

module One = Incr (Zero)

let _ = print_float (Float_u.to_float One.as_float_u)
let _ = print_endline ""
let _ = print_endline One.as_string


let _ = print_endline "Test 4: coercion in and out"

module Double (M : Number) : Number = struct
  let undoubled_float = M.as_float_u
  let as_string = M.as_string ^ "*2"
  let as_float_u = Float_u.add undoubled_float undoubled_float
end

module Two = struct
  let as_int = 2
  let as_float_u = #2.0
  let as_string = "2"
  let as_int64_u = #2L
end

module Four = Double (Two)

let _ = print_float (Float_u.to_float Four.as_float_u)
let _ = print_endline ""
let _ = print_endline Four.as_string

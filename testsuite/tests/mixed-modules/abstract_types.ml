(* TEST
 include stdlib_upstream_compatible;
 reference = "${test_source_directory}/abstract_types.reference";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

open Stdlib_upstream_compatible
external [@layout_poly] id : ('a : any). 'a -> 'a = "%opaque"


let _ = print_endline "Test 1: abstract type : value"

module type S_value = sig
  type t
  val x : t
  val print_t : t -> unit
  val boxed_number : int
  val unboxed_number : int64#
  val boxed_string : string
end

module M_value_abstr : S_value = struct
  type t = string
  let x = "foo"
  let print_t = print_string
  let boxed_number = 10
  let unboxed_number = #20L
  let boxed_string = "bar"
end

let _ = (id M_value_abstr.print_t) (id M_value_abstr.x)
let _ = print_endline ""
let _ = print_int (id M_value_abstr.boxed_number)
let _ = print_endline ""
let _ = print_int (Int64_u.to_int (id M_value_abstr.unboxed_number))
let _ = print_endline ""
let _ = print_endline (id M_value_abstr.boxed_string)


let _ = print_endline "Test 2: abstract type : value with type t := string"

module M_value : S_value with type t := string = struct
  type t = string
  let x = "foo"
  let print_t = print_string
  let boxed_number = 10
  let unboxed_number = #20L
  let boxed_string = "bar"
end

let _ = print_endline (id M_value.x)
let _ = print_int (id M_value.boxed_number)
let _ = print_endline ""
let _ = print_int (Int64_u.to_int (id M_value.unboxed_number))
let _ = print_endline ""
let _ = print_endline (id M_value.boxed_string)


let _ = print_endline "Test 3: abstract type : float64"

module type S_float = sig
  type t : float64
  val x : t
  val print_t : t -> unit
  val boxed_number : int
  val unboxed_number : int64#
  val boxed_string : string
end

module M_unboxed_abstr : S_float = struct
  type t = float#
  let x = #30.0
  let print_t r = print_float (Float_u.to_float r)
  let boxed_number = 40
  let unboxed_number = #50L
  let boxed_string = "baz"
end

let _ = (id M_unboxed_abstr.print_t) (id M_unboxed_abstr.x)
let _ = print_endline ""
let _ = print_int (id M_unboxed_abstr.boxed_number)
let _ = print_endline ""
let _ = print_int (Int64_u.to_int (id M_unboxed_abstr.unboxed_number))
let _ = print_endline ""
let _ = print_endline (id M_unboxed_abstr.boxed_string)


let _ = print_endline "Test 4: abstract type : float64 with type t := float#"

module M_unboxed : S_float with type t := float# = struct
  type t = float#
  let x = #30.0
  let print_t r = print_float (Float_u.to_float r)
  let boxed_number = 40
  let unboxed_number = #50L
  let boxed_string = "baz"
end

let _ = print_float (Float_u.to_float (id M_unboxed.x))
let _ = print_endline ""
let _ = print_int (id M_unboxed.boxed_number)
let _ = print_endline ""
let _ = print_int (Int64_u.to_int (id M_unboxed.unboxed_number))
let _ = print_endline ""
let _ = print_endline (id M_unboxed.boxed_string)

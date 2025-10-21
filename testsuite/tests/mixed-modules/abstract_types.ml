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

type void : void

external id : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly]
external void : unit -> void = "%unbox_unit"

let () = print_endline "Test: abstract type : value"

module type S_value = sig
  type t

  val x : t
  val unboxed_tuple : #(int64# * t)
  val to_string : t -> string
  val unboxed_number : int64#
  val boxed_string : string
  val boxed_number : int
end

module M_value_abstr : S_value = struct
  type t = string

  let x = "foo"
  let to_string = Fun.id
  let boxed_number = 10
  let unboxed_number = #20L
  let unboxed_tuple = #(#30L, "bar")
  let boxed_string = "baz"
end

let () =
  let to_string = id M_value_abstr.to_string in
  let #(thirty, bar) = id M_value_abstr.unboxed_tuple in
  print_endline "Expected: foo bar baz 10 20 30";
  Printf.printf
    "Actual:   %s %s %s %d %d %d\n\n"
    (to_string (id M_value_abstr.x))
    (to_string bar)
    (id M_value_abstr.boxed_string)
    (id M_value_abstr.boxed_number)
    (Int64_u.to_int (id M_value_abstr.unboxed_number))
    (Int64_u.to_int thirty)
;;

let () = print_endline "Test: abstract type : value with type t := string"

module M_value : S_value with type t := string = struct
  type t = string

  let x = "foo"
  let to_string = Fun.id
  let boxed_number = 10
  let unboxed_number = #20L
  let unboxed_tuple = #(#30L, "bar")
  let boxed_string = "baz"
end

let () =
  let #(thirty, bar) = id M_value.unboxed_tuple in
  print_endline "Expected: foo bar baz 10 20 30";
  Printf.printf
    "Actual:   %s %s %s %d %d %d\n\n"
    (id M_value.x)
    bar
    (id M_value.boxed_string)
    (id M_value.boxed_number)
    (Int64_u.to_int (id M_value.unboxed_number))
    (Int64_u.to_int thirty)
;;

let () = print_endline "Test: abstract type : float64"

module type S_float = sig
  type t : float64

  val unboxed_number : int64#
  val boxed_string : string
  val unboxed_tuple : #(string * t)
  val boxed_number : int
  val x : t
  val to_int : t -> int
end

module M_unboxed_abstr : S_float = struct
  type t = float#

  let x = #40.0
  let to_int = Float_u.to_int
  let boxed_number = 50
  let unboxed_tuple = #("foo", #70.0)
  let unboxed_number = #60L
  let boxed_string = "bar"
end

let () =
  let to_int = id M_unboxed_abstr.to_int in
  let #(foo, seventy) = id M_unboxed_abstr.unboxed_tuple in
  print_endline "Expected: foo bar 50 60 40 70";
  Printf.printf
    "Actual:   %s %s %d %d %d %d\n\n"
    foo
    (id M_unboxed_abstr.boxed_string)
    (id M_unboxed_abstr.boxed_number)
    (Int64_u.to_int (id M_unboxed_abstr.unboxed_number))
    (to_int (id M_unboxed_abstr.x))
    (to_int seventy)
;;

let () = print_endline "Test: abstract type : float64 with type t := float#"

module M_unboxed : S_float with type t := float# = struct
  type t = float#

  let x = #40.0
  let to_int = Float_u.to_int
  let boxed_number = 50
  let unboxed_tuple = #("foo", #70.0)
  let unboxed_number = #60L
  let boxed_string = "bar"
end

let () =
  let to_int = id M_unboxed.to_int in
  let #(foo, seventy) = id M_unboxed.unboxed_tuple in
  print_endline "Expected: foo bar 50 60 40 70";
  Printf.printf
    "Actual:   %s %s %d %d %d %d\n\n"
    foo
    (id M_unboxed.boxed_string)
    (id M_unboxed.boxed_number)
    (Int64_u.to_int (id M_unboxed.unboxed_number))
    (to_int (id M_unboxed.x))
    (to_int seventy)
;;

let () = print_endline "Test: abstract type : void"

module type S_void = sig
  type t : void

  val unboxed_number : int64#
  val boxed_string : string
  val x : t
  val unboxed_tuple : #(string * t * int64#)
end

module M_void_abstr : S_void = struct
  type t = void

  let x = void ()
  let unboxed_number = #80L
  let boxed_string = "foo"
  let unboxed_tuple = #("bar", void (), #90L)
end

let () =
  let #(bar, _void_val, ninety) = id M_void_abstr.unboxed_tuple in
  print_endline "Expected: foo bar 80 90";
  Printf.printf
    "Actual:   %s %s %d %d\n\n"
    (id M_void_abstr.boxed_string)
    bar
    (Int64_u.to_int (id M_void_abstr.unboxed_number))
    (Int64_u.to_int ninety)
;;

let () = print_endline "Test: abstract type : void with type t := void"

module M_void : S_void with type t := void = struct
  type t = void

  let x = void ()
  let unboxed_number = #80L
  let boxed_string = "foo"
  let unboxed_tuple = #("bar", void (), #90L)
end

let () =
  let #(bar, _void_val, ninety) = id M_void.unboxed_tuple in
  print_endline "Expected: foo bar 80 90";
  Printf.printf
    "Actual:   %s %s %d %d\n"
    (id M_void.boxed_string)
    bar
    (Int64_u.to_int (id M_void.unboxed_number))
    (Int64_u.to_int ninety)
;;

(* TEST
 include stdlib_upstream_compatible;
 reference = "${test_source_directory}/include_functor.reference";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

open Stdlib_upstream_compatible
external [@layout_poly] id : ('a : any). 'a -> 'a = "%opaque"


let () = print_endline "Test: Basic include functor with unboxed types"

module Make_stats (M : sig val x : float# val name : string end) = struct
  let doubled = Float_u.mul M.x #2.0
  let squared = Float_u.mul M.x M.x
  let description = M.name ^ "_stats"
end

module Stats = struct
  let x = #3.0
  let name = "three"
  include functor Make_stats
end

let () =
  print_endline "Expected: 3.0 6.0 9.0 three_stats";
  Printf.printf
    "Actual:   %.1f %.1f %.1f %s\n\n"
    (Float_u.to_float (id Stats.x))
    (Float_u.to_float (id Stats.doubled))
    (Float_u.to_float (id Stats.squared))
    (id Stats.description)


let () = print_endline "Test: Include functor with shadowing of mixed values"

module Override (M : sig val value : float# val label : string end) = struct
  let original_value = M.value
  let original_label = M.label
  let value = Float_u.add M.value #10.0
  let label = "overridden_" ^ M.label
end

module Combined = struct
  let value = #1.0
  let label = "original"
  include functor Override
end

let () =
  print_endline "Expected: 11.0 overridden_original 1.0";
  Printf.printf
    "Actual:   %.1f %s %.1f\n\n"
    (Float_u.to_float (id Combined.value))
    (id Combined.label)
    (Float_u.to_float (id Combined.original_value))


let () = print_endline "Test: Multiple include functors with mixed types"

module F_3_1 (M : sig val x : int end) = struct
  let x_float = Float_u.of_float (float_of_int M.x)
  let x_string = string_of_int M.x
end

module F_3_2 (M : sig
    val x : int
    val x_float : float#
    val x_string : string
    val y : float#
  end) =
struct
  let y_doubled = Float_u.mul M.y #2.0
  let y_int64 = Int64_u.of_int64 (Int64.of_float (Float_u.to_float M.y))
end

module Multi = struct
  let x = 5
  include functor F_3_1
  let y = #7.0
  include functor F_3_2
end

let () =
  print_endline "Expected: 5.0 5 14.0 7";
  Printf.printf
    "Actual:   %.1f %s %.1f %d\n\n"
    (Float_u.to_float Multi.x_float)
    (id Multi.x_string)
    (Float_u.to_float (id Multi.y_doubled))
    (Int64_u.to_int (id Multi.y_int64))


let () = print_endline "Test: Include functor that adds unboxed fields to input"

module Add_unboxed (M : sig val n : int val s : string end) = struct
  let n_float_u = Float_u.of_float (float_of_int M.n)
  let n_int64_u = Int64_u.of_int M.n
end

module Enhanced = struct
  let n = 42
  let s = "answer"
  include functor Add_unboxed
end

let () =
  print_endline "Expected: 42 answer 42.0 42";
  Printf.printf
    "Actual:   %d %s %.1f %d\n\n"
    (id Enhanced.n)
    (id Enhanced.s)
    (Float_u.to_float (id Enhanced.n_float_u))
    (Int64_u.to_int (id Enhanced.n_int64_u))


let () = print_endline "Test: Nested include functor applications"

module Add_one (M : sig val x : float# val final_string : string end) = struct
  let inner_result = Float_u.add M.x #1.0
  let inner_string = M.final_string ^ "_inner"
end

module Double_inner (M : sig val x : float# end) = struct
  include M
  let final_string = "final"
  include functor Add_one
  let outer_result = Float_u.mul inner_result #2.0
end

module Nested = struct
  let x = #4.0
  include functor Double_inner
end

let () =
  print_endline "Expected: 10.0 final final_inner";
  Printf.printf
    "Actual:   %.1f %s %s\n\n"
    (Float_u.to_float (id Nested.outer_result))
    (id Nested.final_string)
    (id Nested.inner_string)


let () = print_endline "Test: Include with type"

module type S = sig
  type t : float64
  val make : float# -> t
  val extract : t -> float#
end

module Make_wrapper (M : sig end) : S = struct
  type t = float#
  let make x = x
  let extract x = x
end

module W = struct
  include functor Make_wrapper
  let example = make #9.0
end

let () =
  print_endline "Expected: 9.0";
  Printf.printf "Actual:   %.1f\n\n" (Float_u.to_float ((id W.extract) (id W.example)))


let () = print_endline "Test: Value-only input to mixed output"

module Mixed_from_boxed (M : sig val a : int val b : string end) = struct
  include M
  let boxed_value = M.a * 2
  let boxed_string = M.b ^ "_modified"
  let unboxed_float = Float_u.of_float (float_of_int M.a)
  let unboxed_int64 = Int64_u.of_int64 (Int64.of_int M.a)
end

module Mixed_7 = struct
  let a = 15
  let b = "test"
  include functor Mixed_from_boxed
end

let () =
  print_endline "Expected: 30 test_modified 15.0 15";
  Printf.printf
    "Actual:   %d %s %.1f %d\n\n"
    (id Mixed_7.boxed_value)
    (id Mixed_7.boxed_string)
    (Float_u.to_float (id Mixed_7.unboxed_float))
    (Int64_u.to_int (id Mixed_7.unboxed_int64))


let () = print_endline "Test: Mixed input with value-only functor"

module type Value_only_input = sig
  val x : int
  val y : string
end

module type Value_only_output = sig
  val result : int
  val description : string
end

module Value_only_functor (M : Value_only_input) : Value_only_output = struct
  let result = M.x * 3
  let description = M.y ^ "_processed"
end

module Result_8 = struct
  let x = 8
  let y = "data"
  let z = #99.0
  include functor Value_only_functor
end

let () =
  print_endline "Expected: 99.0 24 data_processed";
  Printf.printf
    "Actual:   %.1f %d %s\n"
    (Float_u.to_float (id Result_8.z))
    (id Result_8.result)
    (id Result_8.description)

(* TEST
 modules = "unpack_product_args_c.c";
 reference = "${test_source_directory}/unpack_product_args.reference";
 include stdlib_stable;
 flambda2;
 {
   native;
 }
 {
   bytecode;
 }
*)

external box_int64 : int64# -> (int64[@local_opt]) = "%box_int64"
external box_float : float# -> (float[@local_opt]) = "%box_float"

(* Homogeneous pairs *)

external add_int_int :
  (#(int * int) [@unpacked]) -> int =
  "add_int_int_bytecode" "add_int_int_native"

external add_i64_i64 :
  (#(int64# * int64#) [@unpacked]) -> int64# =
  "add_i64_i64_bytecode" "add_i64_i64_native"

external add_f64_f64 :
  (#(float# * float#) [@unpacked]) -> float# =
  "add_f64_f64_bytecode" "add_f64_f64_native"

let () =
  let result = add_int_int #(10, 32) in
  Printf.printf "add_int_int: %d\n" result

let () =
  let result = add_i64_i64 #(#100L, #23L) in
  Printf.printf "add_i64_i64: %Ld\n" (box_int64 result)

let () =
  let result = add_f64_f64 #(#1.5, #2.5) in
  Printf.printf "add_f64_f64: %f\n" (box_float result)

(* Heterogeneous pairs: all combinations of int, int64#, float# *)

external add_i64_int :
  (#(int64# * int) [@unpacked]) -> int64# =
  "add_i64_int_bytecode" "add_i64_int_native"

external add_int_i64 :
  (#(int * int64#) [@unpacked]) -> int64# =
  "add_int_i64_bytecode" "add_int_i64_native"

external add_f64_int :
  (#(float# * int) [@unpacked]) -> float# =
  "add_f64_int_bytecode" "add_f64_int_native"

external add_int_f64 :
  (#(int * float#) [@unpacked]) -> float# =
  "add_int_f64_bytecode" "add_int_f64_native"

external add_i64_f64 :
  (#(int64# * float#) [@unpacked]) -> float# =
  "add_i64_f64_bytecode" "add_i64_f64_native"

external add_f64_i64 :
  (#(float# * int64#) [@unpacked]) -> float# =
  "add_f64_i64_bytecode" "add_f64_i64_native"

let () =
  let result = add_i64_int #(#100L, 23) in
  Printf.printf "add_i64_int: %Ld\n" (box_int64 result)

let () =
  let result = add_int_i64 #(23, #100L) in
  Printf.printf "add_int_i64: %Ld\n" (box_int64 result)

let () =
  let result = add_f64_int #(#1.5, 2) in
  Printf.printf "add_f64_int: %f\n" (box_float result)

let () =
  let result = add_int_f64 #(2, #1.5) in
  Printf.printf "add_int_f64: %f\n" (box_float result)

let () =
  let result = add_i64_f64 #(#100L, #0.5) in
  Printf.printf "add_i64_f64: %f\n" (box_float result)

let () =
  let result = add_f64_i64 #(#0.5, #100L) in
  Printf.printf "add_f64_i64: %f\n" (box_float result)

(* 3-element product *)

external add_three_ints :
  (#(int * int * int) [@unpacked]) -> int =
  "add_three_ints_bytecode" "add_three_ints_native"

let () =
  let result = add_three_ints #(1, 2, 3) in
  Printf.printf "add_three_ints: %d\n" result

(* Abstract product type *)

module Int_pair : sig
  type t : value & value
  val make : int -> int -> t
  val fst : t -> int
end = struct
  type t = #(int * int)
  let make a b = #(a, b)
  let fst #(a, _) = a
end

external add_abstract :
  (Int_pair.t [@unpacked]) -> int =
  "add_int_int_bytecode" "add_int_int_native"

let () =
  let result = add_abstract (Int_pair.make 10 32) in
  Printf.printf "add_abstract: %d\n" result

(* Multiple unpacked args *)

external add_two_pairs :
  (#(int * int) [@unpacked]) ->
  (#(int * int) [@unpacked]) -> int =
  "add_two_pairs_bytecode" "add_two_pairs_native"

let () =
  let result = add_two_pairs #(1, 2) #(3, 4) in
  Printf.printf "add_two_pairs: %d\n" result

(* Mixed unpacked and regular args *)

external add_mixed :
  int ->
  (#(int * int) [@unpacked]) ->
  int -> int =
  "add_mixed_bytecode" "add_mixed_native"

let () =
  let result = add_mixed 1 #(2, 3) 4 in
  Printf.printf "add_mixed: %d\n" result

(* Nested product: flattened to 3 native args *)

external add_nested :
  (#(int * #(int * int)) [@unpacked]) -> int =
  "add_nested_bytecode" "add_nested_native"

let () =
  let result = add_nested #(10, #(20, 12)) in
  Printf.printf "add_nested: %d\n" result

(* Product with void: void is erased, so 2 native args *)

external add_with_void :
  (#(int * #(unit# * int)) [@unpacked]) -> int =
  "add_with_void_bytecode" "add_with_void_native"

let () =
  let result = add_with_void #(10, #(#(), 32)) in
  Printf.printf "add_with_void: %d\n" result

(* Product of all voids: 0 native args from the product *)

external all_voids :
  (#(unit# * unit#) [@unpacked]) -> int =
  "all_voids_bytecode" "all_voids_native"

let () =
  let result = all_voids #(#(), #()) in
  Printf.printf "all_voids: %d\n" result

(* Unpacked first, regular second *)

external unpacked_first :
  (#(int * int) [@unpacked]) -> int -> int =
  "unpacked_first_bytecode" "unpacked_first_native"

let () =
  let result = unpacked_first #(1, 2) 3 in
  Printf.printf "unpacked_first: %d\n" result

(* Regular first, unpacked second *)

external unpacked_second :
  int -> (#(int * int) [@unpacked]) -> int =
  "unpacked_second_bytecode" "unpacked_second_native"

let () =
  let result = unpacked_second 1 #(2, 3) in
  Printf.printf "unpacked_second: %d\n" result

(* Wide tuple: 9 ints, enough to spill *)

external add_wide :
  (#(int * int * int * int * int * int * int * int * int) [@unpacked])
  -> int =
  "add_wide_bytecode" "add_wide_native"

let () =
  let result =
    add_wide #(1, 20, 300, 4000, 50000, 600000, 7000000, 80000000,
               900000000)
  in
  Printf.printf "add_wide: %d\n" result

(* Wide tuple: 9 float#s, enough to spill *)

external add_wide_float :
  (#(float# * float# * float# * float# * float# * float# * float#
     * float# * float#) [@unpacked])
  -> float# =
  "add_wide_float_bytecode" "add_wide_float_native"

let () =
  let result =
    add_wide_float #(#1., #20., #300., #4000., #50000., #600000.,
                     #7000000., #80000000., #900000000.)
  in
  Printf.printf "add_wide_float: %f\n" (box_float result)

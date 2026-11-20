(* TEST
 reference = "${test_source_directory}/matching.reference";
 flambda2;
 include stdlib_stable;
 include stdlib_upstream_compatible;
 {
   bytecode;
 } {
   native;
 } {
   flags = "-Oclassic";
   native;
 } {
   flags = "-O3";
   native;
 }
*)

open Stdlib_stable
open Stdlib_upstream_compatible

external array_length : ('a : any mod separable). 'a array -> int
  = "%array_length"
[@@layout_poly]

(* Slices of arrays of unboxed elements, as [of_array] in [Ox] *)

let float_slice (arr : float# array) : float# slice =
  Slice.unsafe_of_addr
    (Addr.of_idx arr (Idx_mut.unsafe_create_into_array 0))
    ~len:(Nativeint_u.of_int (array_length arr))

let int32_slice (arr : int32_u array) : int32_u slice =
  Slice.unsafe_of_addr
    (Addr.of_idx arr (Idx_mut.unsafe_create_into_array 0))
    ~len:(Nativeint_u.of_int (array_length arr))

let int8_slice (arr : int8# array) : int8# slice =
  Slice.unsafe_of_addr
    (Addr.of_idx arr (Idx_mut.unsafe_create_into_array 0))
    ~len:(Nativeint_u.of_int (array_length arr))

let int16_slice (arr : int16# array) : int16# slice =
  Slice.unsafe_of_addr
    (Addr.of_idx arr (Idx_mut.unsafe_create_into_array 0))
    ~len:(Nativeint_u.of_int (array_length arr))

let pair_slice (arr : #(int * int) array) : #(int * int) slice =
  Slice.unsafe_of_addr
    (Addr.of_idx arr (Idx_mut.unsafe_create_into_array 0))
    ~len:(Nativeint_u.of_int (array_length arr))

let mixed_slice (arr : #(int * int64_u) array) : #(int * int64_u) slice =
  Slice.unsafe_of_addr
    (Addr.of_idx arr (Idx_mut.unsafe_create_into_array 0))
    ~len:(Nativeint_u.of_int (array_length arr))

external iarray_length : ('a : any mod separable). 'a iarray -> int
  = "%array_length"
[@@layout_poly]

let float_slice_imm (arr : float# iarray) : float# slice_imm =
  Slice_imm.unsafe_of_addr
    (Addr_imm.of_idx arr (Idx_imm.unsafe_create_into_iarray 0))
    ~len:(Nativeint_u.of_int (iarray_length arr))

(* Lengths and shapes *)

let describe_fixed = function
  | [&| |] -> "empty"
  | [&| x |] -> Printf.sprintf "one: %d" x
  | [&| x; y |] -> Printf.sprintf "two: %d %d" x y
  | _ -> "more"

let () =
  List.iter
    (fun a -> print_endline (describe_fixed (Slice.of_array a)))
    [ [||]; [| 1 |]; [| 1; 2 |]; [| 1; 2; 3 |] ]

(* Elements and lengths *)

let classify_fixed = function
  | [&| 0; _; _ |] -> "three, starting with 0"
  | [&| _; _; _ |] -> "three"
  | [&| 0 |] -> "just 0"
  | _ -> "other"

let () =
  List.iter
    (fun a -> print_endline (classify_fixed (Slice.of_array a)))
    [ [| 0; 1; 2 |]; [| 1; 2; 3 |]; [| 0 |]; [| 1 |]; [| 0; 1 |] ]

(* Immutable slices *)

let sum4_imm = function [&: a; b; c; d :] -> a + b + c + d | _ -> -1

let () = Printf.printf "%d\n" (sum4_imm (Slice_imm.of_iarray [: 1; 2; 3; 4 :]))

(* Elements that are pointers *)

let concat3 = function [&| a; b; c |] -> a ^ b ^ c | _ -> ""

let () = print_endline (concat3 (Slice.of_array [| "ab"; "cd"; "ef" |]))

(* Unboxed elements, of different sizes, at different positions *)

let sum3_float = function
  | [&| a; b; c |] -> Float_u.add a (Float_u.add b c)
  | _ -> #0.0

let () =
  Printf.printf "%.1f\n"
    (Float_u.to_float (sum3_float (float_slice [| #1.5; #2.5; #3.0 |])))

let sum4_int32 = function
  | [&| a; b; c; d |] -> Int32_u.(add a (add b (add c d)))
  | _ -> #0l

let () =
  Printf.printf "%ld\n"
    (Int32_u.to_int32 (sum4_int32 (int32_slice [| #1l; #2l; #3l; #4l |])))

let sum5_int8 = function
  | [&| a; b; c; d; e |] ->
      Int8_u.(to_int a + to_int b + to_int c + to_int d + to_int e)
  | _ -> -1

let () =
  Printf.printf "%d\n" (sum5_int8 (int8_slice [| #1s; #2s; #3s; #4s; #5s |]))

let sum3_pairs = function
  | [&| #(a, b); #(c, d); #(e, f) |] -> a + b + c + d + e + f
  | _ -> -1

let () =
  Printf.printf "%d\n"
    (sum3_pairs (pair_slice [| #(1, 10); #(2, 20); #(3, 30) |]))

let sum3_int16 = function
  | [&| a; b; c |] -> Int16_u.(to_int a + to_int b + to_int c)
  | _ -> -1

let () = Printf.printf "%d\n" (sum3_int16 (int16_slice [| #1S; #2S; #300S |]))

let sum2_mixed = function
  | [&| #(i, j); #(k, l) |] ->
      Int64.(add (add (of_int i) (Int64_u.to_int64 j))
               (add (of_int k) (Int64_u.to_int64 l)))
  | _ -> -1L

let () =
  Printf.printf "%Ld\n"
    (sum2_mixed (mixed_slice [| #(1, #10L); #(2, #20L) |]))

let sum2_float_imm = function
  | [&: a; b :] -> Float_u.to_float a +. Float_u.to_float b
  | _ -> 0.

let () =
  Printf.printf "%.1f\n" (sum2_float_imm (float_slice_imm [: #0.5; #1.5 :]))

(* As for mutable record fields, an element is read once, before the guard,
   and later rows use the value read then *)

let read_after_guard_fixed arr =
  match Slice.of_array arr with
  | [&| _ |] when (arr.(0) <- 2; false) -> -1
  | [&| x |] -> x
  | _ -> 0

let () = Printf.printf "%d\n" (read_after_guard_fixed [| 1 |])

(* Dereference patterns on elements *)

let middle_addr = function
  | [&| _; deref_ a; _ |] -> Some a
  | _ -> None

let () =
  let arr = [| 1; 2; 3 |] in
  match middle_addr (Slice.of_array arr) with
  | Some a ->
      Addr.set a 20;
      Printf.printf "%d %d\n" (Addr.get a) arr.(1)
  | None -> print_endline "no middle element"

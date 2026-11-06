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

let rec length = function
  | [&| |] -> 0
  | [&| _; .. as r |] -> 1 + length r

let describe = function
  | [&| |] -> "empty"
  | [&| x |] -> Printf.sprintf "one: %d" x
  | [&| x; y |] -> Printf.sprintf "two: %d %d" x y
  | [&| x; y; .. as r |] ->
      Printf.sprintf "%d, %d and %d more" x y (length r)

let () =
  List.iter
    (fun a -> print_endline (describe (Slice.of_array a)))
    [ [||]; [| 1 |]; [| 1; 2 |]; [| 1; 2; 3; 4; 5 |] ]

(* Closed and open patterns of different lengths *)

let classify = function
  | [&| 0; .. |] -> "starts with 0"
  | [&| _; _; _ |] -> "three elements"
  | [&| _; .. |] -> "something else"
  | [&| |] -> "empty"

let () =
  List.iter
    (fun a -> print_endline (classify (Slice.of_array a)))
    [ [| 0; 1; 2 |]; [| 1; 2; 3 |]; [| 1; 2 |]; [| 0 |]; [||] ]

(* Immutable slices *)

let rec sum_imm = function
  | [&: :] -> 0
  | [&: x; .. as r :] -> x + sum_imm r

let () = Printf.printf "%d\n" (sum_imm (Slice_imm.of_iarray [: 1; 2; 3; 4 :]))

(* Elements that are pointers *)

let rec concat = function
  | [&| |] -> ""
  | [&| s; .. as r |] -> s ^ concat r

let () = print_endline (concat (Slice.of_array [| "ab"; "cd"; "ef" |]))

(* Unboxed elements, of different sizes *)

let rec sum_float = function
  | [&| |] -> #0.0
  | [&| x; .. as r |] -> Float_u.add x (sum_float r)

let () =
  Printf.printf "%.1f\n"
    (Float_u.to_float (sum_float (float_slice [| #1.5; #2.5; #3.0 |])))

let rec sum_int32 = function
  | [&| |] -> #0l
  | [&| x; .. as r |] -> Int32_u.add x (sum_int32 r)

let () =
  Printf.printf "%ld\n"
    (Int32_u.to_int32 (sum_int32 (int32_slice [| #1l; #2l; #3l; #4l |])))

let rec sum_int8 = function
  | [&| |] -> 0
  | [&| x; .. as r |] -> Int8_u.to_int x + sum_int8 r

let () =
  Printf.printf "%d\n"
    (sum_int8 (int8_slice [| #1s; #2s; #3s; #4s; #5s |]))

let rec sum_pairs = function
  | [&| |] -> 0
  | [&| #(x, y); .. as r |] -> x + y + sum_pairs r

let () =
  Printf.printf "%d\n"
    (sum_pairs (pair_slice [| #(1, 10); #(2, 20); #(3, 30) |]))

let rec sum_int16 = function
  | [&| |] -> 0
  | [&| x; .. as r |] -> Int16_u.to_int x + sum_int16 r

let () = Printf.printf "%d\n" (sum_int16 (int16_slice [| #1S; #2S; #300S |]))

let rec sum_mixed = function
  | [&| |] -> 0L
  | [&| #(i, j); .. as r |] ->
      Int64.add (Int64.of_int i) (Int64.add (Int64_u.to_int64 j) (sum_mixed r))

let () =
  Printf.printf "%Ld\n"
    (sum_mixed (mixed_slice [| #(1, #10L); #(2, #20L) |]))

let rec sum_float_imm = function
  | [&: :] -> 0.
  | [&: x; .. as r :] -> Float_u.to_float x +. sum_float_imm r

let () =
  Printf.printf "%.1f\n" (sum_float_imm (float_slice_imm [: #0.5; #1.5 :]))

(* Void elements take no space *)

external void_slice_of_parts
  : #(#(int array * (int array, int) idx_mut) * nativeint_u) -> unit# slice
  = "%identity"

let void_slice n =
  void_slice_of_parts
    #(#([||], Idx_mut.unsafe_create_into_array 0), Nativeint_u.of_int n)

let rec count_void = function
  | [&| |] -> 0
  | [&| #(); .. as r |] -> 1 + count_void r

let () = Printf.printf "%d\n" (count_void (void_slice 4))

(* The rest bound at different positions in an or-pattern *)

let rest_after_marker = function
  | [&| 0; .. as r |] | [&| _; 0; .. as r |] -> length r
  | _ -> -1

let () =
  List.iter
    (fun a -> Printf.printf "%d\n" (rest_after_marker (Slice.of_array a)))
    [ [| 0; 1; 2 |]; [| 1; 0; 2; 3; 4 |]; [| 1; 1 |] ]

(* As for mutable record fields, an element is read once, before the guard,
   and later rows use the value read then *)

let read_after_guard arr =
  match Slice.of_array arr with
  | [&| _; .. |] when (arr.(0) <- 2; false) -> -1
  | [&| x; .. |] -> x
  | [&| |] -> 0

let () = Printf.printf "%d\n" (read_after_guard [| 1 |])

(* Dereference patterns on elements *)

let second_addr = function
  | [&| _; deref_ a; .. |] -> Some a
  | _ -> None

let () =
  let arr = [| 1; 2; 3 |] in
  match second_addr (Slice.of_array arr) with
  | Some a ->
      Addr.set a 20;
      Printf.printf "%d %d\n" (Addr.get a) arr.(1)
  | None -> print_endline "no second element"

(* Elements after the rest *)

let last = function
  | [&| |] -> "none"
  | [&| ..; x |] -> string_of_int x

let () =
  List.iter
    (fun a -> print_endline (last (Slice.of_array a)))
    [ [||]; [| 1 |]; [| 1; 2; 3 |] ]

let ends = function
  | [&| x; .. as r; y |] -> Printf.sprintf "%d..%d, %d between" x y (length r)
  | [&| x |] -> Printf.sprintf "only %d" x
  | [&| |] -> "empty"

let () =
  List.iter
    (fun a -> print_endline (ends (Slice.of_array a)))
    [ [| 1; 2; 3; 4 |]; [| 1; 2 |]; [| 5 |]; [||] ]

(* Prefixes and suffixes of different lengths in one match *)

let mixed = function
  | [&| 0; .. |] -> "starts with 0"
  | [&| ..; 0 |] -> "ends with 0"
  | [&| _; _ |] -> "two"
  | [&| x; ..; y; z |] -> Printf.sprintf "%d %d %d" x y z
  | _ -> "other"

let () =
  List.iter
    (fun a -> print_endline (mixed (Slice.of_array a)))
    [ [| 0; 1 |]; [| 1; 0 |]; [| 1; 2 |]; [| 1; 2; 3 |]; [| 4; 5; 6; 7 |];
      [| 1 |] ]

(* The rest bound before and after an element in an or-pattern *)

let around_zero = function
  | [&| 0; .. as r |] | [&| .. as r; 0 |] -> length r
  | _ -> -1

let () =
  List.iter
    (fun a -> Printf.printf "%d\n" (around_zero (Slice.of_array a)))
    [ [| 0; 1; 2 |]; [| 1; 2; 3; 0 |]; [| 1; 0; 1 |] ]

(* Suffixes of unboxed elements of different sizes *)

let last_float = function
  | [&| ..; x |] -> Float_u.to_float x
  | [&| |] -> 0.

let () =
  Printf.printf "%.1f\n" (last_float (float_slice [| #1.5; #2.5; #3.5 |]))

let last_two_int8 = function
  | [&| ..; x; y |] -> Int8_u.to_int x + Int8_u.to_int y
  | _ -> -1

let () =
  Printf.printf "%d\n" (last_two_int8 (int8_slice [| #1s; #2s; #30s; #40s |]))

(* Immutable slices *)

let last_imm = function
  | [&: ..; x :] -> x
  | [&: :] -> 0

let () = Printf.printf "%d\n" (last_imm (Slice_imm.of_iarray [: 1; 2; 3 :]))

(* Dereference patterns on elements after the rest *)

let () =
  let arr = [| 1; 2; 3 |] in
  match Slice.of_array arr with
  | [&| ..; deref_ a |] ->
      Addr.set a 30;
      Printf.printf "%d %d\n" (Addr.get a) arr.(2)
  | [&| |] -> print_endline "empty"

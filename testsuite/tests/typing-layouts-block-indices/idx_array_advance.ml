(* TEST
 reference = "${test_source_directory}/idx_array_advance.reference";
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

(* Advancing indices of array elements ([Idx_mut.unsafe_advance] and
   [Idx_imm.unsafe_advance]) *)

open Stdlib_stable
open Stdlib_upstream_compatible

(* For each kind of array element, the elements at the positions reached from
   the first element by advancing forwards, then backwards. Indices into arrays
   of [float]s can't be created, since they might be flat float arrays. *)

let () =
  let arr = [| 0; 1; 2; 3; 4 |] in
  let idx0 = Idx_mut.unsafe_create_into_array 0 in
  let idx3 = Idx_mut.unsafe_advance idx0 ~by:3 in
  Printf.printf "int: %d %d %d %d\n"
    (Idx_mut.get arr (Idx_mut.unsafe_advance idx0 ~by:1))
    (Idx_mut.get arr (Idx_mut.unsafe_advance idx0 ~by:2))
    (Idx_mut.get arr idx3)
    (Idx_mut.get arr (Idx_mut.unsafe_advance idx3 ~by:(-2)))

let () =
  let arr = [| "a"; "b"; "c"; "d"; "e" |] in
  let idx0 = Idx_mut.unsafe_create_into_array 0 in
  let idx3 = Idx_mut.unsafe_advance idx0 ~by:3 in
  Printf.printf "string: %s %s %s %s\n"
    (Idx_mut.get arr (Idx_mut.unsafe_advance idx0 ~by:1))
    (Idx_mut.get arr (Idx_mut.unsafe_advance idx0 ~by:2))
    (Idx_mut.get arr idx3)
    (Idx_mut.get arr (Idx_mut.unsafe_advance idx3 ~by:(-2)))

(* Unboxed elements, of different sizes *)

let float_u_array : float# array = [| #0.5; #1.5; #2.5; #3.5; #4.5 |]

let () =
  let idx0 = Idx_mut.unsafe_create_into_array 0 in
  let at idx = Float_u.to_float (Idx_mut.get float_u_array idx) in
  Printf.printf "float#: %.1f %.1f\n"
    (at (Idx_mut.unsafe_advance idx0 ~by:1))
    (at (Idx_mut.unsafe_advance idx0 ~by:4))

let float32_u_array : Float32_u.t array =
  [| #0.5s; #1.5s; #2.5s; #3.5s; #4.5s |]

let () =
  let idx0 = Idx_mut.unsafe_create_into_array 0 in
  let at idx =
    Float32.to_float (Float32_u.to_float32 (Idx_mut.get float32_u_array idx))
  in
  Printf.printf "float32#: %.1f %.1f\n"
    (at (Idx_mut.unsafe_advance idx0 ~by:1))
    (at (Idx_mut.unsafe_advance idx0 ~by:4))

let int8_u_array : int8# array = [| #0s; #1s; #2s; #3s; #4s |]

let () =
  let idx0 = Idx_mut.unsafe_create_into_array 0 in
  let at idx = Int8_u.to_int (Idx_mut.get int8_u_array idx) in
  Printf.printf "int8#: %d %d\n"
    (at (Idx_mut.unsafe_advance idx0 ~by:1))
    (at (Idx_mut.unsafe_advance idx0 ~by:4))

let int16_u_array : int16# array = [| #0S; #1S; #2S; #3S; #4S |]

let () =
  let idx0 = Idx_mut.unsafe_create_into_array 0 in
  let at idx = Int16_u.to_int (Idx_mut.get int16_u_array idx) in
  Printf.printf "int16#: %d %d\n"
    (at (Idx_mut.unsafe_advance idx0 ~by:1))
    (at (Idx_mut.unsafe_advance idx0 ~by:4))

let int32_u_array : int32_u array = [| #0l; #1l; #2l; #3l; #4l |]

let () =
  let idx0 = Idx_mut.unsafe_create_into_array 0 in
  let at idx = Int32_u.to_int (Idx_mut.get int32_u_array idx) in
  Printf.printf "int32#: %d %d\n"
    (at (Idx_mut.unsafe_advance idx0 ~by:1))
    (at (Idx_mut.unsafe_advance idx0 ~by:4))

let int64_u_array : int64_u array = [| #0L; #1L; #2L; #3L; #4L |]

let () =
  let idx0 = Idx_mut.unsafe_create_into_array 0 in
  let at idx = Int64_u.to_int (Idx_mut.get int64_u_array idx) in
  Printf.printf "int64#: %d %d\n"
    (at (Idx_mut.unsafe_advance idx0 ~by:1))
    (at (Idx_mut.unsafe_advance idx0 ~by:4))

let nativeint_u_array : nativeint_u array = [| #0n; #1n; #2n; #3n; #4n |]

let () =
  let idx0 = Idx_mut.unsafe_create_into_array 0 in
  let at idx = Nativeint_u.to_int (Idx_mut.get nativeint_u_array idx) in
  Printf.printf "nativeint#: %d %d\n"
    (at (Idx_mut.unsafe_advance idx0 ~by:1))
    (at (Idx_mut.unsafe_advance idx0 ~by:4))

(* Unboxed products: of values, of elements of different sizes, and
   nested *)

let pair_array : #(string * int) array =
  [| #("a", 0); #("b", 1); #("c", 2); #("d", 3) |]

let () =
  let idx0 = Idx_mut.unsafe_create_into_array 0 in
  let #(s, i) = Idx_mut.get pair_array (Idx_mut.unsafe_advance idx0 ~by:2) in
  Printf.printf "#(string * int): %s %d\n" s i

let mixed_array : #(int * int32_u * float#) array =
  [| #(0, #0l, #0.5); #(1, #1l, #1.5); #(2, #2l, #2.5); #(3, #3l, #3.5) |]

let () =
  let idx0 = Idx_mut.unsafe_create_into_array 0 in
  let #(i, j, f) =
    Idx_mut.get mixed_array (Idx_mut.unsafe_advance idx0 ~by:3)
  in
  Printf.printf "#(int * int32# * float#): %d %d %.1f\n" i (Int32_u.to_int j)
    (Float_u.to_float f)

let nested_array : #(int * #(string * int)) array =
  [| #(0, #("a", 10)); #(1, #("b", 11)); #(2, #("c", 12)) |]

let () =
  let idx0 = Idx_mut.unsafe_create_into_array 0 in
  let #(i, #(s, j)) =
    Idx_mut.get nested_array (Idx_mut.unsafe_advance idx0 ~by:2)
  in
  Printf.printf "#(int * #(string * int)): %d %s %d\n" i s j

(* Advancing by integers of every index kind *)

let () =
  let arr = [| 0; 1; 2; 3; 4; 5; 6 |] in
  let idx0 = Idx_mut.unsafe_create_into_array 0 in
  Printf.printf "index kinds: %d %d %d %d %d %d\n"
    (Idx_mut.get arr (Idx_mut.unsafe_advance idx0 ~by:1))
    (Idx_mut.get arr (Idx_mut.unsafe_advance_indexed_by_int8 idx0 ~by:#2s))
    (Idx_mut.get arr (Idx_mut.unsafe_advance_indexed_by_int16 idx0 ~by:#3S))
    (Idx_mut.get arr (Idx_mut.unsafe_advance_indexed_by_int32 idx0 ~by:#4l))
    (Idx_mut.get arr (Idx_mut.unsafe_advance_indexed_by_int64 idx0 ~by:#5L))
    (Idx_mut.get arr
       (Idx_mut.unsafe_advance_indexed_by_nativeint idx0 ~by:#6n))

(* Advancing by an unknown amount, which may be zero or negative *)

let[@inline never] advance idx by = Idx_mut.unsafe_advance idx ~by

let () =
  let arr = [| 0; 1; 2; 3; 4 |] in
  let idx2 = Idx_mut.unsafe_create_into_array 2 in
  Printf.printf "unknown amounts: %d %d %d\n"
    (Idx_mut.get arr (advance idx2 0))
    (Idx_mut.get arr (advance idx2 (-2)))
    (Idx_mut.get arr (advance (advance idx2 2) (-1)))

(* Writing through an advanced index *)

let () =
  let arr = [| #0l; #1l; #2l; #3l |] in
  let idx =
    Idx_mut.unsafe_advance (Idx_mut.unsafe_create_into_array 0) ~by:2
  in
  Idx_mut.set arr idx #20l;
  let at i =
    Int32_u.to_int (Idx_mut.get arr (Idx_mut.unsafe_create_into_array i))
  in
  Printf.printf "write: %d %d %d %d\n" (at 0) (at 1) (at 2) (at 3)

(* Immutable arrays *)

let () =
  let iarr : int16# iarray = [: #0S; #1S; #2S; #3S :] in
  let idx0 = Idx_imm.unsafe_create_into_iarray 0 in
  Printf.printf "iarray: %d %d\n"
    (Int16_u.to_int (Idx_imm.get iarr (Idx_imm.unsafe_advance idx0 ~by:3)))
    (Int16_u.to_int
       (Idx_imm.get iarr
          (Idx_imm.unsafe_advance_indexed_by_nativeint idx0 ~by:#1n)))

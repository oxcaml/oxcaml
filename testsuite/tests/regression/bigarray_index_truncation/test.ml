(* TEST *)

(* Indexing a bigarray with byte-sized elements by an int obtained by
   truncating a nativeint (or an int64) whose bit 63 is set used to read from
   the wrong address in native code: the sign extension performed by
   [Nativeint.to_int] / [Int64.to_int] was lost when the tagged index was
   fused into the address computation. *)

open Bigarray

type bytes_ba = (char, int8_unsigned_elt, c_layout) Array1.t

type shorts_ba = (int, int16_unsigned_elt, c_layout) Array1.t

let[@inline never] unsafe_get_nativeint (b : bytes_ba) (x : nativeint) =
  Array1.unsafe_get b (Nativeint.to_int x)

let[@inline never] unsafe_get_int64 (b : bytes_ba) (x : int64) =
  Array1.unsafe_get b (Int64.to_int x)

let[@inline never] get_nativeint (b : bytes_ba) (x : nativeint) =
  Array1.get b (Nativeint.to_int x)

let[@inline never] unsafe_set_nativeint (b : bytes_ba) (x : nativeint) c =
  Array1.unsafe_set b (Nativeint.to_int x) c

let[@inline never] unsafe_get_int16 (b : shorts_ba) (x : nativeint) =
  Array1.unsafe_get b (Nativeint.to_int x)

let () =
  let bytes =
    Array1.of_array char c_layout
      [| 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h' |]
  in
  let shorts =
    Array1.of_array int16_unsigned c_layout
      [| 100; 101; 102; 103; 104; 105; 106; 107 |]
  in
  let x = Sys.opaque_identity (Nativeint.add Nativeint.min_int 5n) in
  let x64 = Sys.opaque_identity (Int64.add Int64.min_int 5L) in
  Printf.printf "Nativeint.to_int x = %d\n%!" (Nativeint.to_int x);
  Printf.printf "Int64.to_int x64 = %d\n%!" (Int64.to_int x64);
  Printf.printf "unsafe_get (nativeint index) = %c\n%!"
    (unsafe_get_nativeint bytes x);
  Printf.printf "unsafe_get (int64 index) = %c\n%!"
    (unsafe_get_int64 bytes x64);
  Printf.printf "get (nativeint index) = %c\n%!" (get_nativeint bytes x);
  Printf.printf "unsafe_get (constant index) = %c\n%!"
    (Array1.unsafe_get bytes
       (Nativeint.to_int (Nativeint.add Nativeint.min_int 5n)));
  unsafe_set_nativeint bytes x 'F';
  Printf.printf "after unsafe_set (nativeint index) = %c\n%!"
    (Array1.get bytes 5);
  Printf.printf "unsafe_get int16 (nativeint index) = %d\n%!"
    (unsafe_get_int16 shorts x)

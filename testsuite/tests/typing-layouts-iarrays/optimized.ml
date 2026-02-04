(* TEST
 include stdlib_upstream_compatible;
 include stdlib_stable;
 flambda2;
 {
   native;
 }{
   bytecode;
 }{
   flags = "-O3";
   native;
 }{
   flags = "-Oclassic";
   native;
 }
*)

(* Tests for unboxed immutable arrays *)

external[@layout_poly] length : ('a : any mod separable). 'a iarray -> int = "%array_length"
external[@layout_poly] get : ('a : any mod separable). 'a iarray -> int -> 'a = "%array_safe_get"
external[@layout_poly] unsafe_get : ('a : any mod separable). 'a iarray -> int -> 'a = "%array_unsafe_get"
external[@layout_poly] set : ('a : any mod separable). 'a array -> int -> 'a -> unit = "%array_safe_set"
external[@layout_poly] make_mutable : ('a : any mod separable). int -> 'a -> 'a array = "%makearray_dynamic"
external[@layout_poly] to_iarray : ('a : any mod separable). 'a array -> 'a iarray = "%array_to_iarray"

module Float_u = Stdlib_upstream_compatible.Float_u
module Int32_u = Stdlib_upstream_compatible.Int32_u
module Int64_u = Stdlib_upstream_compatible.Int64_u
module Nativeint_u = Stdlib_upstream_compatible.Nativeint_u
module Float32 = Stdlib_stable.Float32
module Float32_u = Stdlib_stable.Float32_u
module Int8_u = Stdlib_stable.Int8_u
module Int16_u = Stdlib_stable.Int16_u

let test_float () =
  Printf.printf "Testing float#\n";
  let len = 5 in
  let arr_mut = make_mutable len (Float_u.of_float 0.0) in
  for i = 0 to len - 1 do
    set arr_mut i (Float_u.of_float (float_of_int i))
  done;
  let arr = to_iarray arr_mut in
  Printf.printf "Length: %d\n" (length arr);
  Printf.printf "Elements: ";
  for i = 0 to len - 1 do
    Printf.printf "%.1f " (Float_u.to_float (get arr i));
  done;
  Printf.printf "\n";
  Printf.printf "Unsafe get: ";
  for i = 0 to len - 1 do
    Printf.printf "%.1f " (Float_u.to_float (unsafe_get arr i));
  done;
  Printf.printf "\n";
  (* Test out of bounds *)
  try
    let _ = get arr len in
    Printf.printf "Out of bounds access did not raise exception\n"
  with 
  | Invalid_argument _ ->
    Printf.printf "Out of bounds access raised Invalid_argument\n"
  

let test_int32 () =
  Printf.printf "Testing int32#\n";
  let len = 5 in
  let arr_mut = make_mutable len (Int32_u.of_int32 0l) in
  for i = 0 to len - 1 do
    set arr_mut i (Int32_u.of_int32 (Int32.of_int i))
  done;
  let arr = to_iarray arr_mut in
  Printf.printf "Length: %d\n" (length arr);
  Printf.printf "Elements: ";
  for i = 0 to len - 1 do
    Printf.printf "%ld " (Int32_u.to_int32 (get arr i));
  done;
  Printf.printf "\n";
  Printf.printf "Unsafe get: ";
  for i = 0 to len - 1 do
    Printf.printf "%ld " (Int32_u.to_int32 (unsafe_get arr i));
  done;
  Printf.printf "\n";
  (* Test out of bounds *)
  try
    let _ = get arr len in
    Printf.printf "Out of bounds access did not raise exception\n"
  with 
  | Invalid_argument _ ->
    Printf.printf "Out of bounds access raised Invalid_argument\n"
  

let test_int64 () =
  Printf.printf "Testing int64#\n";
  let len = 5 in
  let arr_mut = make_mutable len (Int64_u.of_int64 0L) in
  for i = 0 to len - 1 do
    set arr_mut i (Int64_u.of_int64 (Int64.of_int i))
  done;
  let arr = to_iarray arr_mut in
  Printf.printf "Length: %d\n" (length arr);
  Printf.printf "Elements: ";
  for i = 0 to len - 1 do
    Printf.printf "%Ld " (Int64_u.to_int64 (get arr i));
  done;
  Printf.printf "\n";
  Printf.printf "Unsafe get: ";
  for i = 0 to len - 1 do
    Printf.printf "%Ld " (Int64_u.to_int64 (unsafe_get arr i));
  done;
  Printf.printf "\n";
  (* Test out of bounds *)
  try
    let _ = get arr len in
    Printf.printf "Out of bounds access did not raise exception\n"
  with 
  | Invalid_argument _ ->
    Printf.printf "Out of bounds access raised Invalid_argument\n"
  

let test_nativeint () =
  Printf.printf "Testing nativeint#\n";
  let len = 5 in
  let arr_mut = make_mutable len (Nativeint_u.of_nativeint 0n) in
  for i = 0 to len - 1 do
    set arr_mut i (Nativeint_u.of_nativeint (Nativeint.of_int i))
  done;
  let arr = to_iarray arr_mut in
  Printf.printf "Length: %d\n" (length arr);
  Printf.printf "Elements: ";
  for i = 0 to len - 1 do
    Printf.printf "%nd " (Nativeint_u.to_nativeint (get arr i));
  done;
  Printf.printf "\n";
  Printf.printf "Unsafe get: ";
  for i = 0 to len - 1 do
    Printf.printf "%nd " (Nativeint_u.to_nativeint (unsafe_get arr i));
  done;
  Printf.printf "\n";
  (* Test out of bounds *)
  try
    let _ = get arr len in
    Printf.printf "Out of bounds access did not raise exception\n"
  with 
  | Invalid_argument _ ->
    Printf.printf "Out of bounds access raised Invalid_argument\n"
  

let test_float32 () =
  Printf.printf "Testing float32#\n";
  let len = 5 in
  let arr_mut = make_mutable len (Float32_u.of_float32 Float32.zero) in
  for i = 0 to len - 1 do
    set arr_mut i (Float32_u.of_float32 (Float32.of_int i))
  done;
  let arr = to_iarray arr_mut in
  Printf.printf "Length: %d\n" (length arr);
  Printf.printf "Elements: ";
  for i = 0 to len - 1 do
    Printf.printf "%.1f " (Float32.to_float (Float32_u.to_float32 (get arr i)));
  done;
  Printf.printf "\n";
  Printf.printf "Unsafe get: ";
  for i = 0 to len - 1 do
    Printf.printf "%.1f " (Float32.to_float (Float32_u.to_float32 (unsafe_get arr i)));
  done;
  Printf.printf "\n";
  (* Test out of bounds *)
  try
    let _ = get arr len in
    Printf.printf "Out of bounds access did not raise exception\n"
  with 
  | Invalid_argument _ ->
    Printf.printf "Out of bounds access raised Invalid_argument\n"
  

(* CR small-ints: enable when int8 and int16 arrays are supported *)

(*
let test_int8 () =
  Printf.printf "Testing int8#\n";
  let len = 5 in
  let arr_mut = make_mutable len (Int8_u.of_int8 Int8.zero) in
  for i = 0 to len - 1 do
    set arr_mut i (Int8_u.of_int8 (Int8.of_int i))
  done;
  let arr = to_iarray arr_mut in
  Printf.printf "Length: %d\n" (length arr);
  Printf.printf "Elements: ";
  for i = 0 to len - 1 do
    Printf.printf "%d " (Int8.to_int (Int8_u.to_int8 (get arr i)));
  done;
  Printf.printf "\n";
  Printf.printf "Unsafe get: ";
  for i = 0 to len - 1 do
    Printf.printf "%d " (Int8.to_int (Int8_u.to_int8 (unsafe_get arr i)));
  done;
  Printf.printf "\n";
  (* Test out of bounds *)
  try
    let _ = get arr len in
    Printf.printf "Out of bounds access did not raise exception\n"
  with 
  | Invalid_argument _ ->
    Printf.printf "Out of bounds access raised Invalid_argument\n"
  

let test_int16 () =
  Printf.printf "Testing int16#\n";
  let len = 5 in
  let arr_mut = make_mutable len (Int16_u.of_int16 Int16.zero) in
  for i = 0 to len - 1 do
    set arr_mut i (Int16_u.of_int16 (Int16.of_int i))
  done;
  let arr = to_iarray arr_mut in
  Printf.printf "Length: %d\n" (length arr);
  Printf.printf "Elements: ";
  for i = 0 to len - 1 do
    Printf.printf "%d " (Int16.to_int (Int16_u.to_int16 (get arr i)));
  done;
  Printf.printf "\n";
  Printf.printf "Unsafe get: ";
  for i = 0 to len - 1 do
    Printf.printf "%d " (Int16.to_int (Int16_u.to_int16 (unsafe_get arr i)));
  done;
  Printf.printf "\n";
  (* Test out of bounds *)
  try
    let _ = get arr len in
    Printf.printf "Out of bounds access did not raise exception\n"
  with 
  | Invalid_argument _ ->
    Printf.printf "Out of bounds access raised Invalid_argument\n"
*)

let () =
  test_float ();
  test_int32 ();
  test_int64 ();
  test_nativeint ();
  test_float32 ();

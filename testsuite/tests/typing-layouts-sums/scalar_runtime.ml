(* TEST
 include stdlib_stable;
 { bytecode; } { native; }
*)

open Stdlib_stable

external box_int32 : int32_u -> int32 = "%box_int32"
external box_nativeint : nativeint_u -> nativeint = "%box_nativeint"
external unbox_nativeint : nativeint -> nativeint_u = "%unbox_nativeint"

type narrow = Byte of int8# | Short of int16# | Single of float32_u
  [@@unboxed]

let[@inline never] read_narrow = function
  | Byte byte -> Int32.of_int (Int8_u.to_int byte)
  | Short short -> Int32.of_int (Int16_u.to_int short)
  | Single single -> box_int32 (Float32_u.to_bits single)

let () =
  assert (read_narrow (Byte (Int8_u.of_int (-128))) = -128l);
  assert (read_narrow (Short (Int16_u.of_int (-32768))) = -32768l);
  assert (read_narrow (Single (Float32_u.of_bits #0xffc0_002al))
          = 0xffc0_002al)

type wide = Word of nativeint_u | Untagged of int# | Widened of float32_u
  [@@unboxed]

let[@inline never] read_wide = function
  | Word word -> Int64.of_nativeint (box_nativeint word)
  | Untagged number -> Int64.of_int (Int_u.to_int number)
  | Widened single -> Int64.of_int32 (box_int32 (Float32_u.to_bits single))

let () =
  assert (read_wide (Word (unbox_nativeint Nativeint.min_int))
          = Int64.of_nativeint Nativeint.min_int);
  assert (read_wide (Untagged (Int_u.of_int min_int)) = Int64.of_int min_int);
  assert (read_wide (Widened (Float32_u.of_bits #0x8000_0000l))
          = -2147483648L)

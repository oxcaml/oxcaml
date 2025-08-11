(* TEST
 {
   flags = "-extension layouts_beta -extension small_numbers_beta";
   native;
 }
*)

(* Preliminaries to minimize deps *)

external i8_of_int : int -> int8# = "%int8#_of_int"
external i16_of_int : int -> int16# = "%int16#_of_int"
external icaml_of_int : int -> int# = "%int#_of_int"
external i8_equal : int8# -> int8# -> bool = "%int8#_equal"
external i16_equal : int16# -> int16# -> bool = "%int16#_equal"
external int_equal : int -> int -> bool = "%int_equal"
external icaml_equal : int# -> int# -> bool = "%int#_equal"
external i32_equal : int32# -> int32# -> bool = "%int32#_equal"
external i64_equal : int64# -> int64# -> bool = "%int64#_equal"
external isize_equal : nativeint# -> nativeint# -> bool = "%nativeint#_equal"
external f64_equal : float# -> float# -> bool = "%float#_ordered_and_equal"
external f32_equal : float32# -> float32# -> bool = "%float32#_ordered_and_equal"
external box_f64 : float# -> float = "%float_of_float#"
external box_f32 : float32# -> float32 = "%float32_of_float32#"

external float32_to_bits
  :  (float32[@local_opt])
  -> int32
  = "caml_float32_to_bits_bytecode" "caml_float32_to_bits"
[@@unboxed] [@@noalloc] [@@builtin]

let f64_bits_equal x y =
  Int64.equal (Int64.bits_of_float (box_f64 x)) (Int64.bits_of_float (box_f64 y))
;;

let f32_bits_equal x y =
  Int32.equal (float32_to_bits (box_f32 x)) (float32_to_bits (box_f32 y))
;;

external ( + ) : nativeint# -> nativeint# -> nativeint# = "%nativeint#_add"

(* The test itself starts here *)

(* For the real API we don't plan to expose the type equality, but it
   makes it easier to write the test below. *)
type ('a : any) t = nativeint#

external read : ('a : any mod external_). 'a t -> 'a = "%peek" [@@layout_poly]
external write : ('a : any mod external_). 'a t -> 'a -> unit = "%poke" [@@layout_poly]

external calloc
  :  count:(int[@untagged])
  -> size:(int[@untagged])
  -> nativeint#
  = "caml_no_bytecode_impl" "calloc"
[@@noalloc]

let () =
  let buf = calloc ~count:1 ~size:51 in
  let i64 = #9999999999L in
  let isize = #123456n in
  let i32 = #400l in
  let i16 = i16_of_int 0x1234 in
  let i8 = i8_of_int 0x12 in
  let int = min_int in
  let icaml = icaml_of_int max_int in
  let f64 = #0.1234 in
  let f32 = -#0.1234s in
  write (buf + #0n) i64;
  write (buf + #8n) f64;
  write (buf + #16n) isize;
  write (buf + #24n) int;
  write (buf + #32n) icaml;
  write (buf + #40n) i32;
  write (buf + #44n) f32;
  write (buf + #48n) i16;
  write (buf + #50n) i8;
  assert (i64_equal i64 (read (buf + #0n)));
  assert (f64_equal f64 (read (buf + #8n)));
  assert (f64_bits_equal f64 (read (buf + #8n)));
  assert (isize_equal isize (read (buf + #16n)));
  assert (int_equal int (read (buf + #24n)));
  assert (icaml_equal icaml (read (buf + #32n)));
  assert (i32_equal i32 (read (buf + #40n)));
  assert (f32_equal f32 (read (buf + #44n)));
  assert (f32_bits_equal f32 (read (buf + #44n)));
  assert (i16_equal i16 (read (buf + #48n)));
  assert (i8_equal i8 (read (buf + #50n)))
;;

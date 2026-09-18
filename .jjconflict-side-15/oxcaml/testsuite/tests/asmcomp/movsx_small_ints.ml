(* TEST
 arch_amd64;
 not-windows;
 not-macos;
 include stdlib_stable;
 flambda2;
 flags = "-internal-assembler";
 native;
*)

(* Test demonstrating miscompilations in [emit_movsx] in
   [backend/x86_binary_emitter.ml]. The buggy emitter encodes [movsbq]
   (sign-extend byte to 64 bits) without REX.W, which decodes as [movsbl]
   and zero-extends the upper 32 bits of the destination.

   Reading negative values from an [int8# array] (and analogously from an
   [int16# array]) therefore produces the wrong result when the program is
   compiled with [-internal-assembler]. *)

module Int8_u = Stdlib_stable.Int8_u
module Int16_u = Stdlib_stable.Int16_u

external[@layout_poly] makearray_dynamic :
  ('a : any mod separable). int -> 'a -> 'a array = "%makearray_dynamic"

external unsafe_set_int8 : int8# array -> int -> int8# -> unit
  = "%array_unsafe_set"

external unsafe_get_int8 : int8# array -> int -> int8#
  = "%array_unsafe_get"

external unsafe_set_int16 : int16# array -> int -> int16# -> unit
  = "%array_unsafe_set"

external unsafe_get_int16 : int16# array -> int -> int16#
  = "%array_unsafe_get"

let[@inline never] read_int8 arr i = Int8_u.to_int (unsafe_get_int8 arr i)

let[@inline never] read_int16 arr i = Int16_u.to_int (unsafe_get_int16 arr i)

let test_int8 values =
  let arr = makearray_dynamic (List.length values) (Int8_u.of_int 0) in
  List.iteri (fun i v -> unsafe_set_int8 arr i (Int8_u.of_int v)) values;
  List.iteri
    (fun i v -> Printf.printf "int8: in %d, out %d\n" v (read_int8 arr i))
    values

let test_int16 values =
  let arr = makearray_dynamic (List.length values) (Int16_u.of_int 0) in
  List.iteri (fun i v -> unsafe_set_int16 arr i (Int16_u.of_int v)) values;
  List.iteri
    (fun i v -> Printf.printf "int16: in %d, out %d\n" v (read_int16 arr i))
    values

let () =
  test_int8 [-1; -128; 127; 0];
  test_int16 [-1; -32768; 32767; 0]

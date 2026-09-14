(* TEST
 arch_amd64;
 not-windows;
 not-macos;
 include stdlib_stable;
 flambda2;
 flags = "-internal-assembler -nodynlink";
 native;
*)

(* Test for [emit_movsx] and [emit_MOVZX] in [backend/x86_binary_emitter.ml]
   with RIP-relative memory operands.

   With [-nodynlink], 8- and 16-bit loads from statically-allocated data
   (module-level [int8#] and [int16#] constants, string literals) are emitted
   with a RIP-relative address, i.e. a [Mem64_RIP] operand rather than a
   [Mem] operand. The binary emitter used to reject such operands for
   sign- and zero-extending loads with an assertion failure. *)

module Int8_u = Stdlib_stable.Int8_u
module Int16_u = Stdlib_stable.Int16_u

external opaque_int8 : int8# -> int8# = "%opaque"
external opaque_int16 : int16# -> int16# = "%opaque"

(* The module initialiser loads these from static data with [movsbq] and
   [movswq]. *)
let x8 = opaque_int8 (-#1s)
let x16 = opaque_int16 (-#1S)

let[@inline never] get8 () = Int8_u.to_int x8
let[@inline never] get16 () = Int16_u.to_int x16

(* Loads at constant offsets from a string literal. *)
let s = "\xff\x80\x7f\x00"

let () =
  Printf.printf "int8#: %d\n" (get8 ());
  Printf.printf "int16#: %d\n" (get16 ());
  Printf.printf "unsafe_get: %d\n" (Char.code (String.unsafe_get s 0));
  Printf.printf "get_int8: %d\n" (String.get_int8 s 0);
  Printf.printf "get_uint16_le: %d\n" (String.get_uint16_le s 0);
  Printf.printf "get_int16_le: %d\n" (String.get_int16_le s 0)

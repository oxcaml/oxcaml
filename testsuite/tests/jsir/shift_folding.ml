(* TEST
 flambda2;
 {
   native;
 }{
   (* Under js_of_ocaml, ints and nativeints are 32 bits wide. *)
   reference = "${test_source_directory}/shift_folding.jsir-output.reference";
   jsir-js;
 }{
   jsir-wasm;
 }
*)

(* Checks that Flambda 2 folds shifts of constants using the width of the
   target's integers, rather than that of the machine the compiler was
   configured for. A shift by at least that width is unspecified, and Flambda 2
   folds it to zero. Under js_of_ocaml this starts at 32: such shifts used to
   be computed with the compiler's own [Int32] shifts, whose result is
   unspecified and differs between the native and bytecode compilers. *)

[@@@ocaml.flambda_o3]

let o = Sys.opaque_identity

let () =
  Printf.printf "int lsl: %d %d %d %d %d %d %d\n" (1 lsl 31) (1 lsl 32)
    (1 lsl 33) (1 lsl 62) (1 lsl 63) (1 lsl 64) (1 lsl 65);
  Printf.printf "int asr: %d %d %d %d %d %d %d\n" (-8 asr 31) (-8 asr 32)
    (-8 asr 33) (-8 asr 62) (-8 asr 63) (-8 asr 64) (-8 asr 65);
  Printf.printf "int lsr: %d %d %d %d %d %d %d\n" (-8 lsr 31) (-8 lsr 32)
    (-8 lsr 33) (-8 lsr 62) (-8 lsr 63) (-8 lsr 64) (-8 lsr 65);
  Printf.printf "nativeint shift_left: %nd %nd %nd %nd %nd %nd %nd\n"
    (Nativeint.shift_left 1n 31)
    (Nativeint.shift_left 1n 32)
    (Nativeint.shift_left 1n 33)
    (Nativeint.shift_left 1n 62)
    (Nativeint.shift_left 1n 63)
    (Nativeint.shift_left 1n 64)
    (Nativeint.shift_left 1n 65);
  Printf.printf "nativeint shift_right: %nd %nd %nd %nd %nd %nd %nd\n"
    (Nativeint.shift_right (-8n) 31)
    (Nativeint.shift_right (-8n) 32)
    (Nativeint.shift_right (-8n) 33)
    (Nativeint.shift_right (-8n) 62)
    (Nativeint.shift_right (-8n) 63)
    (Nativeint.shift_right (-8n) 64)
    (Nativeint.shift_right (-8n) 65);
  Printf.printf "nativeint shift_right_logical: %nd %nd %nd %nd %nd %nd %nd\n"
    (Nativeint.shift_right_logical (-8n) 31)
    (Nativeint.shift_right_logical (-8n) 32)
    (Nativeint.shift_right_logical (-8n) 33)
    (Nativeint.shift_right_logical (-8n) 62)
    (Nativeint.shift_right_logical (-8n) 63)
    (Nativeint.shift_right_logical (-8n) 64)
    (Nativeint.shift_right_logical (-8n) 65)

(* Shifts by less than the width of every target are folded to what the
   target computes. *)
let () =
  Printf.printf "in range: %b %b %b %b %b %b\n"
    (1 lsl 31 = o 1 lsl o 31)
    (-8 asr 31 = o (-8) asr o 31)
    (-8 lsr 31 = o (-8) lsr o 31)
    (Nativeint.equal
       (Nativeint.shift_left 1n 31)
       (Nativeint.shift_left (o 1n) (o 31)))
    (Nativeint.equal
       (Nativeint.shift_right (-8n) 31)
       (Nativeint.shift_right (o (-8n)) (o 31)))
    (Nativeint.equal
       (Nativeint.shift_right_logical (-8n) 31)
       (Nativeint.shift_right_logical (o (-8n)) (o 31)))

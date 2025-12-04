(* TEST
 flags = "-g";
 native;
*)

(* This test checks that the obj_dup primitive is simplified correctly. *)

external globalize_float : local_ float -> float = "%obj_dup"

(* The obj_dup primitive in this code will be split into two primitives
   (unbox_float then box_float). The approach before OxCaml PR#4923
   failed to track that the boxing primitive used the result of the
   unboxing primitive, causing the unboxing primitive to disappear and
   raising a fatal error at compilation about an unbound variable.
*)
let of_float__local n = globalize_float n

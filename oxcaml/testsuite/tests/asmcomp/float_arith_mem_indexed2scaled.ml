(* TEST
 arch_amd64;
 flambda2;
 flags = "-fno-avx -fno-avx2";
 native;
*)

(* Regression test for [Cfg_selection.pseudoregs_for_operation] handling of
   [Ifloatarithmem] with an [Iindexed2scaled] addressing mode.

   [select_floatarith] rewrites a commutative float operation whose right
   operand is a [float array] load into [Ifloatarithmem], whose memory operand
   is that load.  A variable-index float array access has the addressing mode
   [Iindexed2scaled (scale, disp)], which needs *two* registers (base and
   index).  [pseudoregs_for_operation] used to return only
   [| res.(0); arg.(1) |] for [Ifloatarithmem], dropping [arg.(2)], and the
   amd64 emitter then raised [Invalid_argument "index out of bounds"].

   [flags] forces the two-operand (non-AVX) form: with three-operand float ops
   the constraint function returns [Use_default_exn] and the bug is not hit. *)

let add_at (a : float array) (i : int) (j : int) = a.(i) +. a.(j)
let mul_at (a : float array) (i : int) (j : int) = a.(i) *. a.(j)

let () =
  let a = [| 1.5; 2.5; 3.5 |] in
  Printf.printf "add = %.2f\n" (add_at a 0 1);
  Printf.printf "mul = %.2f\n" (mul_at a 1 2)

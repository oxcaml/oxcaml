(* TEST *)

(* Regression test for a CFG peephole bug: when folding [lsl] followed by
   [mul] (or the converse), a shift amount of 63 made the computation of the
   combined immediate overflow on the host ([1 lsl 63] evaluates to [0]),
   incorrectly folding the multiplication to [0]. *)

let[@inline never] shift_then_mul (x : int64) =
  Int64.mul (Int64.shift_left x 63) 3L

let[@inline never] mul_then_shift (x : int64) =
  Int64.shift_left (Int64.mul x 3L) 63

let () =
  assert (Int64.equal (shift_then_mul 1L) Int64.min_int);
  assert (Int64.equal (mul_then_shift 1L) Int64.min_int)

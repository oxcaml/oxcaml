module F = Float_ops.Float_u

let float_u_test expr res =
  Format.printf "%s = %f\n" expr (F.to_float res)

let compare_test f1 f2 =
  let res = match F.compare (F.of_float f1) (F.of_float f2) with
    | 1 -> ">"
    | 0 -> "="
    | -1 -> "<"
    | _ -> assert false
  in
  Format.printf "%f %s %f\n" f1 res f2

let print_int_newline n = print_int n; print_newline ()

let () =
  float_u_test "3.7 + 2.3" (F.add (#3.7) (#2.3));
  float_u_test "3.7 - 2.3" (F.sub (#3.7) (#2.3));
  float_u_test "3.7 * 2.3" (F.mul (#3.7) (#2.3));
  float_u_test "3.7 / 2.3" (F.div (#3.7) (#2.3));
  float_u_test "abs(-3.7)" (F.abs (F.of_float (-3.7)));
  float_u_test "neg(3.7)" (F.neg (#3.7));
  compare_test 3.7 2.3;
  compare_test 2.3 3.7;
  compare_test 3.7 3.7;
  compare_test nan 3.7;
  compare_test (-.2.3) nan;
  compare_test (-.infinity) nan;
  compare_test (-.infinity) (-.nan);
  compare_test infinity (-.nan);
  compare_test infinity (-.infinity);
  compare_test (-.infinity) infinity;
  compare_test 0. (-.0.);
  compare_test nan nan

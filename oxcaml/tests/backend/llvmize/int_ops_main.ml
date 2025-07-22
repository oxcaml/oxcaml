let int_test expr res = Format.printf "%s = %d\n" expr res

let () =
  print_endline "Intop:";
  int_test "(-15) + 4" (Int_ops.add ());
  int_test "(-15) - 4" (Int_ops.sub ());
  int_test "(-15) * 4" (Int_ops.mul ());
  int_test "(-15) / 4" (Int_ops.div ());
  int_test "(-15) mod 4" (Int_ops.mod_ ());
  int_test "(-15) land 4" (Int_ops.land_ ());
  int_test "(-15) lor 4" (Int_ops.lor_ ());
  int_test "(-15) lxor 4" (Int_ops.lxor_ ());
  int_test "lnot (-15)" (Int_ops.lnot_ ());
  int_test "(-15) lsl 4" (Int_ops.lsl_ ());
  int_test "(-15) lsr 4" (Int_ops.lsr_ ());
  int_test "(-15) asr 4" (Int_ops.asr_ ());
  print_endline "Intop_imm:";
  int_test "(-15) + 4" (Int_ops.add_imm ());
  int_test "(-15) - 4" (Int_ops.sub_imm ());
  int_test "(-15) * 4" (Int_ops.mul_imm ());
  int_test "(-15) / 4" (Int_ops.div_imm ());
  int_test "(-15) mod 4" (Int_ops.mod_imm ());
  int_test "(-15) land 4" (Int_ops.land_imm ());
  int_test "(-15) lor 4" (Int_ops.lor_imm ());
  int_test "(-15) lxor 4" (Int_ops.lxor_imm ());
  int_test "(-15) lsl 4" (Int_ops.lsl_imm ());
  int_test "(-15) lsr 4" (Int_ops.lsr_imm ());
  int_test "(-15) asr 4" (Int_ops.asr_imm ())

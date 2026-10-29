(* TEST
 modules = "shifted_xor_oracle.c";
 include stdlib_upstream_compatible;
 compare_programs = "false";
 arch64;
 {
   flags = "-O2 -extension-universe beta";
   native;
 } {
   flags = "-O3 -extension-universe beta";
   native;
 }
*)

(* ACTUAL COMPILER: regression for tagged self-xor with constant shifts.
   The reference uses unsigned C arithmetic, independent of Cmm lowering.
   The input matrix is the same edge and random words as the original
   scalar-float investigation. *)

external oracle : int -> int -> int -> bool -> int -> int
  = "shifted_xor_oracle" [@@noalloc]

external raw_oracle : int64 -> int64 -> int -> bool -> int64
  = "shifted_xor_raw_oracle"

external raw_left_oracle : int64 -> int64 -> int -> int64
  = "shifted_xor_raw_left_oracle"

module I = Stdlib_upstream_compatible.Int64_u

type unboxed_case = {
  raw_name : string;
  raw_shift : int;
  raw_right : bool;
  raw_same : bool;
  raw_apply : int64 -> int64 -> int64;
}

type case = {
  name : string;
  shift : int;
  right : bool;
  same : bool;
  apply : int -> int -> int;
}

let[@inline never] left_same_lr_0 a _b = a lxor (a lsl 0)
let[@inline never] left_same_rl_0 a _b = (a lsl 0) lxor a
let[@inline never] left_different_lr_0 a b = a lxor (b lsl 0)
let[@inline never] left_different_rl_0 a b = (b lsl 0) lxor a
let[@inline never] right_same_lr_0 a _b = a lxor (a lsr 0)
let[@inline never] right_same_rl_0 a _b = (a lsr 0) lxor a
let[@inline never] right_different_lr_0 a b = a lxor (b lsr 0)
let[@inline never] right_different_rl_0 a b = (b lsr 0) lxor a
let[@inline never] left_same_lr_1 a _b = a lxor (a lsl 1)
let[@inline never] left_same_rl_1 a _b = (a lsl 1) lxor a
let[@inline never] left_different_lr_1 a b = a lxor (b lsl 1)
let[@inline never] left_different_rl_1 a b = (b lsl 1) lxor a
let[@inline never] right_same_lr_1 a _b = a lxor (a lsr 1)
let[@inline never] right_same_rl_1 a _b = (a lsr 1) lxor a
let[@inline never] right_different_lr_1 a b = a lxor (b lsr 1)
let[@inline never] right_different_rl_1 a b = (b lsr 1) lxor a
let[@inline never] left_same_lr_7 a _b = a lxor (a lsl 7)
let[@inline never] left_same_rl_7 a _b = (a lsl 7) lxor a
let[@inline never] left_different_lr_7 a b = a lxor (b lsl 7)
let[@inline never] left_different_rl_7 a b = (b lsl 7) lxor a
let[@inline never] right_same_lr_7 a _b = a lxor (a lsr 7)
let[@inline never] right_same_rl_7 a _b = (a lsr 7) lxor a
let[@inline never] right_different_lr_7 a b = a lxor (b lsr 7)
let[@inline never] right_different_rl_7 a b = (b lsr 7) lxor a
let[@inline never] left_same_lr_13 a _b = a lxor (a lsl 13)
let[@inline never] left_same_rl_13 a _b = (a lsl 13) lxor a
let[@inline never] left_different_lr_13 a b = a lxor (b lsl 13)
let[@inline never] left_different_rl_13 a b = (b lsl 13) lxor a
let[@inline never] right_same_lr_13 a _b = a lxor (a lsr 13)
let[@inline never] right_same_rl_13 a _b = (a lsr 13) lxor a
let[@inline never] right_different_lr_13 a b = a lxor (b lsr 13)
let[@inline never] right_different_rl_13 a b = (b lsr 13) lxor a
let[@inline never] left_same_lr_17 a _b = a lxor (a lsl 17)
let[@inline never] left_same_rl_17 a _b = (a lsl 17) lxor a
let[@inline never] left_different_lr_17 a b = a lxor (b lsl 17)
let[@inline never] left_different_rl_17 a b = (b lsl 17) lxor a
let[@inline never] right_same_lr_17 a _b = a lxor (a lsr 17)
let[@inline never] right_same_rl_17 a _b = (a lsr 17) lxor a
let[@inline never] right_different_lr_17 a b = a lxor (b lsr 17)
let[@inline never] right_different_rl_17 a b = (b lsr 17) lxor a
let[@inline never] left_same_lr_31 a _b = a lxor (a lsl 31)
let[@inline never] left_same_rl_31 a _b = (a lsl 31) lxor a
let[@inline never] left_different_lr_31 a b = a lxor (b lsl 31)
let[@inline never] left_different_rl_31 a b = (b lsl 31) lxor a
let[@inline never] right_same_lr_31 a _b = a lxor (a lsr 31)
let[@inline never] right_same_rl_31 a _b = (a lsr 31) lxor a
let[@inline never] right_different_lr_31 a b = a lxor (b lsr 31)
let[@inline never] right_different_rl_31 a b = (b lsr 31) lxor a
let[@inline never] left_same_lr_32 a _b = a lxor (a lsl 32)
let[@inline never] left_same_rl_32 a _b = (a lsl 32) lxor a
let[@inline never] left_different_lr_32 a b = a lxor (b lsl 32)
let[@inline never] left_different_rl_32 a b = (b lsl 32) lxor a
let[@inline never] right_same_lr_32 a _b = a lxor (a lsr 32)
let[@inline never] right_same_rl_32 a _b = (a lsr 32) lxor a
let[@inline never] right_different_lr_32 a b = a lxor (b lsr 32)
let[@inline never] right_different_rl_32 a b = (b lsr 32) lxor a
let[@inline never] left_same_lr_61 a _b = a lxor (a lsl 61)
let[@inline never] left_same_rl_61 a _b = (a lsl 61) lxor a
let[@inline never] left_different_lr_61 a b = a lxor (b lsl 61)
let[@inline never] left_different_rl_61 a b = (b lsl 61) lxor a
let[@inline never] right_same_lr_61 a _b = a lxor (a lsr 61)
let[@inline never] right_same_rl_61 a _b = (a lsr 61) lxor a
let[@inline never] right_different_lr_61 a b = a lxor (b lsr 61)
let[@inline never] right_different_rl_61 a b = (b lsr 61) lxor a
let[@inline never] left_same_lr_62 a _b = a lxor (a lsl 62)
let[@inline never] left_same_rl_62 a _b = (a lsl 62) lxor a
let[@inline never] left_different_lr_62 a b = a lxor (b lsl 62)
let[@inline never] left_different_rl_62 a b = (b lsl 62) lxor a
let[@inline never] right_same_lr_62 a _b = a lxor (a lsr 62)
let[@inline never] right_same_rl_62 a _b = (a lsr 62) lxor a
let[@inline never] right_different_lr_62 a b = a lxor (b lsr 62)
let[@inline never] right_different_rl_62 a b = (b lsr 62) lxor a
let[@inline never] left_same_lr_63 a _b = a lxor (a lsl 63)
let[@inline never] left_same_rl_63 a _b = (a lsl 63) lxor a
let[@inline never] left_different_lr_63 a b = a lxor (b lsl 63)
let[@inline never] left_different_rl_63 a b = (b lsl 63) lxor a
let[@inline never] right_same_lr_63 a _b = a lxor (a lsr 63)
let[@inline never] right_same_rl_63 a _b = (a lsr 63) lxor a
let[@inline never] right_different_lr_63 a b = a lxor (b lsr 63)
let[@inline never] right_different_rl_63 a b = (b lsr 63) lxor a

let cases = [|
  { name = "left_same_lr_0"; shift = 0; right = false; same = true; apply = left_same_lr_0 };
  { name = "left_same_rl_0"; shift = 0; right = false; same = true; apply = left_same_rl_0 };
  { name = "left_different_lr_0"; shift = 0; right = false; same = false; apply = left_different_lr_0 };
  { name = "left_different_rl_0"; shift = 0; right = false; same = false; apply = left_different_rl_0 };
  { name = "right_same_lr_0"; shift = 0; right = true; same = true; apply = right_same_lr_0 };
  { name = "right_same_rl_0"; shift = 0; right = true; same = true; apply = right_same_rl_0 };
  { name = "right_different_lr_0"; shift = 0; right = true; same = false; apply = right_different_lr_0 };
  { name = "right_different_rl_0"; shift = 0; right = true; same = false; apply = right_different_rl_0 };
  { name = "left_same_lr_1"; shift = 1; right = false; same = true; apply = left_same_lr_1 };
  { name = "left_same_rl_1"; shift = 1; right = false; same = true; apply = left_same_rl_1 };
  { name = "left_different_lr_1"; shift = 1; right = false; same = false; apply = left_different_lr_1 };
  { name = "left_different_rl_1"; shift = 1; right = false; same = false; apply = left_different_rl_1 };
  { name = "right_same_lr_1"; shift = 1; right = true; same = true; apply = right_same_lr_1 };
  { name = "right_same_rl_1"; shift = 1; right = true; same = true; apply = right_same_rl_1 };
  { name = "right_different_lr_1"; shift = 1; right = true; same = false; apply = right_different_lr_1 };
  { name = "right_different_rl_1"; shift = 1; right = true; same = false; apply = right_different_rl_1 };
  { name = "left_same_lr_7"; shift = 7; right = false; same = true; apply = left_same_lr_7 };
  { name = "left_same_rl_7"; shift = 7; right = false; same = true; apply = left_same_rl_7 };
  { name = "left_different_lr_7"; shift = 7; right = false; same = false; apply = left_different_lr_7 };
  { name = "left_different_rl_7"; shift = 7; right = false; same = false; apply = left_different_rl_7 };
  { name = "right_same_lr_7"; shift = 7; right = true; same = true; apply = right_same_lr_7 };
  { name = "right_same_rl_7"; shift = 7; right = true; same = true; apply = right_same_rl_7 };
  { name = "right_different_lr_7"; shift = 7; right = true; same = false; apply = right_different_lr_7 };
  { name = "right_different_rl_7"; shift = 7; right = true; same = false; apply = right_different_rl_7 };
  { name = "left_same_lr_13"; shift = 13; right = false; same = true; apply = left_same_lr_13 };
  { name = "left_same_rl_13"; shift = 13; right = false; same = true; apply = left_same_rl_13 };
  { name = "left_different_lr_13"; shift = 13; right = false; same = false; apply = left_different_lr_13 };
  { name = "left_different_rl_13"; shift = 13; right = false; same = false; apply = left_different_rl_13 };
  { name = "right_same_lr_13"; shift = 13; right = true; same = true; apply = right_same_lr_13 };
  { name = "right_same_rl_13"; shift = 13; right = true; same = true; apply = right_same_rl_13 };
  { name = "right_different_lr_13"; shift = 13; right = true; same = false; apply = right_different_lr_13 };
  { name = "right_different_rl_13"; shift = 13; right = true; same = false; apply = right_different_rl_13 };
  { name = "left_same_lr_17"; shift = 17; right = false; same = true; apply = left_same_lr_17 };
  { name = "left_same_rl_17"; shift = 17; right = false; same = true; apply = left_same_rl_17 };
  { name = "left_different_lr_17"; shift = 17; right = false; same = false; apply = left_different_lr_17 };
  { name = "left_different_rl_17"; shift = 17; right = false; same = false; apply = left_different_rl_17 };
  { name = "right_same_lr_17"; shift = 17; right = true; same = true; apply = right_same_lr_17 };
  { name = "right_same_rl_17"; shift = 17; right = true; same = true; apply = right_same_rl_17 };
  { name = "right_different_lr_17"; shift = 17; right = true; same = false; apply = right_different_lr_17 };
  { name = "right_different_rl_17"; shift = 17; right = true; same = false; apply = right_different_rl_17 };
  { name = "left_same_lr_31"; shift = 31; right = false; same = true; apply = left_same_lr_31 };
  { name = "left_same_rl_31"; shift = 31; right = false; same = true; apply = left_same_rl_31 };
  { name = "left_different_lr_31"; shift = 31; right = false; same = false; apply = left_different_lr_31 };
  { name = "left_different_rl_31"; shift = 31; right = false; same = false; apply = left_different_rl_31 };
  { name = "right_same_lr_31"; shift = 31; right = true; same = true; apply = right_same_lr_31 };
  { name = "right_same_rl_31"; shift = 31; right = true; same = true; apply = right_same_rl_31 };
  { name = "right_different_lr_31"; shift = 31; right = true; same = false; apply = right_different_lr_31 };
  { name = "right_different_rl_31"; shift = 31; right = true; same = false; apply = right_different_rl_31 };
  { name = "left_same_lr_32"; shift = 32; right = false; same = true; apply = left_same_lr_32 };
  { name = "left_same_rl_32"; shift = 32; right = false; same = true; apply = left_same_rl_32 };
  { name = "left_different_lr_32"; shift = 32; right = false; same = false; apply = left_different_lr_32 };
  { name = "left_different_rl_32"; shift = 32; right = false; same = false; apply = left_different_rl_32 };
  { name = "right_same_lr_32"; shift = 32; right = true; same = true; apply = right_same_lr_32 };
  { name = "right_same_rl_32"; shift = 32; right = true; same = true; apply = right_same_rl_32 };
  { name = "right_different_lr_32"; shift = 32; right = true; same = false; apply = right_different_lr_32 };
  { name = "right_different_rl_32"; shift = 32; right = true; same = false; apply = right_different_rl_32 };
  { name = "left_same_lr_61"; shift = 61; right = false; same = true; apply = left_same_lr_61 };
  { name = "left_same_rl_61"; shift = 61; right = false; same = true; apply = left_same_rl_61 };
  { name = "left_different_lr_61"; shift = 61; right = false; same = false; apply = left_different_lr_61 };
  { name = "left_different_rl_61"; shift = 61; right = false; same = false; apply = left_different_rl_61 };
  { name = "right_same_lr_61"; shift = 61; right = true; same = true; apply = right_same_lr_61 };
  { name = "right_same_rl_61"; shift = 61; right = true; same = true; apply = right_same_rl_61 };
  { name = "right_different_lr_61"; shift = 61; right = true; same = false; apply = right_different_lr_61 };
  { name = "right_different_rl_61"; shift = 61; right = true; same = false; apply = right_different_rl_61 };
  { name = "left_same_lr_62"; shift = 62; right = false; same = true; apply = left_same_lr_62 };
  { name = "left_same_rl_62"; shift = 62; right = false; same = true; apply = left_same_rl_62 };
  { name = "left_different_lr_62"; shift = 62; right = false; same = false; apply = left_different_lr_62 };
  { name = "left_different_rl_62"; shift = 62; right = false; same = false; apply = left_different_rl_62 };
  { name = "right_same_lr_62"; shift = 62; right = true; same = true; apply = right_same_lr_62 };
  { name = "right_same_rl_62"; shift = 62; right = true; same = true; apply = right_same_rl_62 };
  { name = "right_different_lr_62"; shift = 62; right = true; same = false; apply = right_different_lr_62 };
  { name = "right_different_rl_62"; shift = 62; right = true; same = false; apply = right_different_rl_62 };
  { name = "left_same_lr_63"; shift = 63; right = false; same = true; apply = left_same_lr_63 };
  { name = "left_same_rl_63"; shift = 63; right = false; same = true; apply = left_same_rl_63 };
  { name = "left_different_lr_63"; shift = 63; right = false; same = false; apply = left_different_lr_63 };
  { name = "left_different_rl_63"; shift = 63; right = false; same = false; apply = left_different_rl_63 };
  { name = "right_same_lr_63"; shift = 63; right = true; same = true; apply = right_same_lr_63 };
  { name = "right_same_rl_63"; shift = 63; right = true; same = true; apply = right_same_rl_63 };
  { name = "right_different_lr_63"; shift = 63; right = true; same = false; apply = right_different_lr_63 };
  { name = "right_different_rl_63"; shift = 63; right = true; same = false; apply = right_different_rl_63 };
|]

(* Unboxed inputs force register-valued Cmm operands. The boxed adapters
   let the test runner store the workers in an ordinary array. *)
let[@inline never] unboxed_left_same_lr_0 (a : int64_u) (_b : int64_u) : int64_u =
  I.logxor a (I.shift_left a 0)
let[@inline never] unboxed_left_same_rl_0 (a : int64_u) (_b : int64_u) : int64_u =
  I.logxor (I.shift_left a 0) a
let[@inline never] unboxed_left_different_lr_0 (a : int64_u) (b : int64_u) : int64_u =
  I.logxor a (I.shift_left b 0)
let[@inline never] unboxed_left_different_rl_0 (a : int64_u) (b : int64_u) : int64_u =
  I.logxor (I.shift_left b 0) a
let[@inline never] unboxed_left_same_lr_13 (a : int64_u) (_b : int64_u) : int64_u =
  I.logxor a (I.shift_left a 13)
let[@inline never] unboxed_left_same_rl_13 (a : int64_u) (_b : int64_u) : int64_u =
  I.logxor (I.shift_left a 13) a
let[@inline never] unboxed_left_different_lr_13 (a : int64_u) (b : int64_u) : int64_u =
  I.logxor a (I.shift_left b 13)
let[@inline never] unboxed_left_different_rl_13 (a : int64_u) (b : int64_u) : int64_u =
  I.logxor (I.shift_left b 13) a
let[@inline never] unboxed_left_same_lr_63 (a : int64_u) (_b : int64_u) : int64_u =
  I.logxor a (I.shift_left a 63)
let[@inline never] unboxed_left_same_rl_63 (a : int64_u) (_b : int64_u) : int64_u =
  I.logxor (I.shift_left a 63) a
let[@inline never] unboxed_left_different_lr_63 (a : int64_u) (b : int64_u) : int64_u =
  I.logxor a (I.shift_left b 63)
let[@inline never] unboxed_left_different_rl_63 (a : int64_u) (b : int64_u) : int64_u =
  I.logxor (I.shift_left b 63) a
let[@inline never] unboxed_right_same_lr_0 (a : int64_u) (_b : int64_u) : int64_u =
  I.logxor a (I.shift_right_logical a 0)
let[@inline never] unboxed_right_same_rl_0 (a : int64_u) (_b : int64_u) : int64_u =
  I.logxor (I.shift_right_logical a 0) a
let[@inline never] unboxed_right_different_lr_0 (a : int64_u) (b : int64_u) : int64_u =
  I.logxor a (I.shift_right_logical b 0)
let[@inline never] unboxed_right_different_rl_0 (a : int64_u) (b : int64_u) : int64_u =
  I.logxor (I.shift_right_logical b 0) a
let[@inline never] unboxed_right_same_lr_7 (a : int64_u) (_b : int64_u) : int64_u =
  I.logxor a (I.shift_right_logical a 7)
let[@inline never] unboxed_right_same_rl_7 (a : int64_u) (_b : int64_u) : int64_u =
  I.logxor (I.shift_right_logical a 7) a
let[@inline never] unboxed_right_different_lr_7 (a : int64_u) (b : int64_u) : int64_u =
  I.logxor a (I.shift_right_logical b 7)
let[@inline never] unboxed_right_different_rl_7 (a : int64_u) (b : int64_u) : int64_u =
  I.logxor (I.shift_right_logical b 7) a
let[@inline never] unboxed_right_same_lr_63 (a : int64_u) (_b : int64_u) : int64_u =
  I.logxor a (I.shift_right_logical a 63)
let[@inline never] unboxed_right_same_rl_63 (a : int64_u) (_b : int64_u) : int64_u =
  I.logxor (I.shift_right_logical a 63) a
let[@inline never] unboxed_right_different_lr_63 (a : int64_u) (b : int64_u) : int64_u =
  I.logxor a (I.shift_right_logical b 63)
let[@inline never] unboxed_right_different_rl_63 (a : int64_u) (b : int64_u) : int64_u =
  I.logxor (I.shift_right_logical b 63) a

let unboxed_cases = [|
  { raw_name = "unboxed_left_same_lr_0"; raw_shift = 0; raw_right = false;
    raw_same = true;
    raw_apply = (fun a b -> I.to_int64 (unboxed_left_same_lr_0 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_left_same_rl_0"; raw_shift = 0; raw_right = false;
    raw_same = true;
    raw_apply = (fun a b -> I.to_int64 (unboxed_left_same_rl_0 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_left_different_lr_0"; raw_shift = 0; raw_right = false;
    raw_same = false;
    raw_apply = (fun a b -> I.to_int64 (unboxed_left_different_lr_0 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_left_different_rl_0"; raw_shift = 0; raw_right = false;
    raw_same = false;
    raw_apply = (fun a b -> I.to_int64 (unboxed_left_different_rl_0 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_left_same_lr_13"; raw_shift = 13; raw_right = false;
    raw_same = true;
    raw_apply = (fun a b -> I.to_int64 (unboxed_left_same_lr_13 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_left_same_rl_13"; raw_shift = 13; raw_right = false;
    raw_same = true;
    raw_apply = (fun a b -> I.to_int64 (unboxed_left_same_rl_13 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_left_different_lr_13"; raw_shift = 13; raw_right = false;
    raw_same = false;
    raw_apply = (fun a b -> I.to_int64 (unboxed_left_different_lr_13 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_left_different_rl_13"; raw_shift = 13; raw_right = false;
    raw_same = false;
    raw_apply = (fun a b -> I.to_int64 (unboxed_left_different_rl_13 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_left_same_lr_63"; raw_shift = 63; raw_right = false;
    raw_same = true;
    raw_apply = (fun a b -> I.to_int64 (unboxed_left_same_lr_63 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_left_same_rl_63"; raw_shift = 63; raw_right = false;
    raw_same = true;
    raw_apply = (fun a b -> I.to_int64 (unboxed_left_same_rl_63 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_left_different_lr_63"; raw_shift = 63; raw_right = false;
    raw_same = false;
    raw_apply = (fun a b -> I.to_int64 (unboxed_left_different_lr_63 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_left_different_rl_63"; raw_shift = 63; raw_right = false;
    raw_same = false;
    raw_apply = (fun a b -> I.to_int64 (unboxed_left_different_rl_63 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_right_same_lr_0"; raw_shift = 0; raw_right = true;
    raw_same = true;
    raw_apply = (fun a b -> I.to_int64 (unboxed_right_same_lr_0 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_right_same_rl_0"; raw_shift = 0; raw_right = true;
    raw_same = true;
    raw_apply = (fun a b -> I.to_int64 (unboxed_right_same_rl_0 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_right_different_lr_0"; raw_shift = 0; raw_right = true;
    raw_same = false;
    raw_apply = (fun a b -> I.to_int64 (unboxed_right_different_lr_0 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_right_different_rl_0"; raw_shift = 0; raw_right = true;
    raw_same = false;
    raw_apply = (fun a b -> I.to_int64 (unboxed_right_different_rl_0 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_right_same_lr_7"; raw_shift = 7; raw_right = true;
    raw_same = true;
    raw_apply = (fun a b -> I.to_int64 (unboxed_right_same_lr_7 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_right_same_rl_7"; raw_shift = 7; raw_right = true;
    raw_same = true;
    raw_apply = (fun a b -> I.to_int64 (unboxed_right_same_rl_7 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_right_different_lr_7"; raw_shift = 7; raw_right = true;
    raw_same = false;
    raw_apply = (fun a b -> I.to_int64 (unboxed_right_different_lr_7 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_right_different_rl_7"; raw_shift = 7; raw_right = true;
    raw_same = false;
    raw_apply = (fun a b -> I.to_int64 (unboxed_right_different_rl_7 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_right_same_lr_63"; raw_shift = 63; raw_right = true;
    raw_same = true;
    raw_apply = (fun a b -> I.to_int64 (unboxed_right_same_lr_63 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_right_same_rl_63"; raw_shift = 63; raw_right = true;
    raw_same = true;
    raw_apply = (fun a b -> I.to_int64 (unboxed_right_same_rl_63 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_right_different_lr_63"; raw_shift = 63; raw_right = true;
    raw_same = false;
    raw_apply = (fun a b -> I.to_int64 (unboxed_right_different_lr_63 (I.of_int64 a) (I.of_int64 b))) };
  { raw_name = "unboxed_right_different_rl_63"; raw_shift = 63; raw_right = true;
    raw_same = false;
    raw_apply = (fun a b -> I.to_int64 (unboxed_right_different_rl_63 (I.of_int64 a) (I.of_int64 b))) };
|]

let[@inline never] offset_plus a = a lxor ((a lsl 13) + 1)
let[@inline never] offset_minus a = ((a lsl 17) - 1) lxor a
let[@inline never] dynamic a k = a lxor (a lsl k)
let[@inline never] raw_asr a b = Int64.logxor a (Int64.shift_right b 7)
let[@inline never] raw_asr_reversed a b =
  Int64.logxor (Int64.shift_right b 7) a
let[@inline never] raw_lsr a b =
  Int64.logxor a (Int64.shift_right_logical b 7)
let[@inline never] raw_lsr_reversed a b =
  Int64.logxor (Int64.shift_right_logical b 7) a

let[@inline never] recurrence seed count =
  let rec loop s n =
    if n = 0 then s
    else
      let s = s lxor (s lsl 13) in
      let s = s lxor (s lsr 7) in
      let s = s lxor (s lsl 17) in
      loop s (n - 1)
  in
  loop seed count

let[@inline never] observe log marker value =
  log := marker :: !log;
  value

let[@inline never] same_effect log a =
  let x = observe log 1 a in
  x lxor (x lsl 13)

let[@inline never] different_effect log a b =
  let x = observe log 1 a in
  let y = observe log 2 b in
  (y lsl 13) lxor x

(* Operand evaluation order is not specified here; the number of evaluations
   and the returned value are specified. *)
let[@inline never] nested_left log a b =
  (observe log 1 a) lxor ((observe log 2 b) lsl (observe log 3 13))

let[@inline never] nested_left_reversed log a b =
  ((observe log 2 b) lsl (observe log 3 13)) lxor (observe log 1 a)

let[@inline never] nested_right log a b =
  (observe log 1 a) lxor ((observe log 2 b) lsr (observe log 3 7))

exception Expected_raise

let[@inline never] raise_marker log =
  log := 9 :: !log;
  raise Expected_raise

let[@inline never] nested_raise_shift log a b =
  (observe log 1 a) lxor ((observe log 2 b) lsl (raise_marker log))

let[@inline never] nested_raise_data log a =
  (observe log 1 a) lxor ((raise_marker log) lsl (observe log 2 13))

let input_words () =
  let values = ref [0L; 1L; -1L; 2L; -2L; 3L; -3L;
    Int64.min_int; Int64.max_int; Int64.succ Int64.min_int;
    Int64.pred Int64.max_int; Int64.of_int min_int; Int64.of_int max_int;
    Int64.of_int (min_int + 1); Int64.of_int (max_int - 1); 0x5555555555555555L;
    0xaaaaaaaaaaaaaaaaL; 0x7fffffffffffffffL; 0x8000000000000001L] in
  for k = 0 to 63 do
    let bit = Int64.shift_left 1L k in
    values := bit :: Int64.pred bit :: Int64.succ bit ::
      Int64.neg bit :: !values
  done;
  let rng = Random.State.make [|0x5ca1a; 0x63; 0x13; 0x17|] in
  for _ = 1 to 128 do
    let hi = Int64.shift_left (Int64.of_int (Random.State.bits rng)) 34 in
    let mid = Int64.shift_left (Int64.of_int (Random.State.bits rng)) 4 in
    let lo = Int64.of_int (Random.State.int rng 16) in
    values := Int64.logor hi (Int64.logor mid lo) :: !values
  done;
  Array.of_list !values

let check name actual expected =
  if actual <> expected then
    failwith (Printf.sprintf "%s: got %d, expected %d" name actual expected)

let () =
  let words = input_words () in
  let tagged = Array.map Int64.to_int words in
  let n = Array.length tagged in
  Array.iter (fun c ->
    Array.iteri (fun i a ->
      let b = words.((i * 73 + 19) mod n) in
      let rhs = if c.raw_same then a else b in
      let expected =
        if c.raw_right then raw_oracle a rhs c.raw_shift false
        else raw_left_oracle a rhs c.raw_shift
      in
      let actual = c.raw_apply a b in
      if actual <> expected then
        failwith (Printf.sprintf "%s: got %Lx, expected %Lx"
          c.raw_name actual expected)
    ) words
  ) unboxed_cases;
  Array.iter (fun c ->
    Array.iteri (fun i a ->
      let b = tagged.((i * 73 + 19) mod n) in
      let rhs = if c.same then a else b in
      check c.name (c.apply a b) (oracle a rhs c.shift c.right 0)
    ) tagged
  ) cases;
  Array.iteri (fun i a ->
    let b = tagged.((i + 1) mod n) in
    let raw_a = words.(i) in
    let raw_b = words.((i + 1) mod n) in
    let check_raw name actual expected =
      if actual <> expected then
        failwith (Printf.sprintf "%s: got %Lx, expected %Lx" name actual expected)
    in
    check_raw "raw asr" (raw_asr raw_a raw_b)
      (raw_oracle raw_a raw_b 7 true);
    check_raw "raw asr reversed" (raw_asr_reversed raw_a raw_b)
      (raw_oracle raw_a raw_b 7 true);
    check_raw "raw lsr" (raw_lsr raw_a raw_b)
      (raw_oracle raw_a raw_b 7 false);
    check_raw "raw lsr reversed" (raw_lsr_reversed raw_a raw_b)
      (raw_oracle raw_a raw_b 7 false);
    check "offset plus" (offset_plus a) (oracle a a 13 false 1);
    check "offset minus" (offset_minus a) (oracle a a 17 false (-1));
    for k = 0 to 63 do
      check "dynamic" (dynamic a k) (oracle a a k false 0)
    done;
    List.iter (fun count ->
      let expected = ref a in
      for _ = 1 to count do
        expected := oracle !expected !expected 13 false 0;
        expected := oracle !expected !expected 7 true 0;
        expected := oracle !expected !expected 17 false 0
      done;
      check "recurrence" (recurrence a count) !expected
    ) [0; 1; 2; 3; 17; 1000];
    let log = ref [] in
    check "same effect" (same_effect log a) (oracle a a 13 false 0);
    assert (List.rev !log = [1]);
    log := [];
    check "different effect" (different_effect log a b) (oracle a b 13 false 0);
    assert (List.rev !log = [1; 2]);
    let check_nested name f shift right =
      log := [];
      check name (f log a b) (oracle a b shift right 0);
      assert (List.sort compare !log = [1; 2; 3])
    in
    check_nested "nested left" nested_left 13 false;
    check_nested "nested left reversed" nested_left_reversed 13 false;
    check_nested "nested right" nested_right 7 true;
    let check_raise f =
      log := [];
      let raised =
        try ignore (f ()); false with Expected_raise -> true
      in
      assert raised;
      let count marker = List.length (List.filter ((=) marker) !log) in
      assert (count 9 = 1);
      assert (count 1 <= 1 && count 2 <= 1);
      assert (List.for_all (fun marker -> marker = 1 || marker = 2 || marker = 9) !log)
    in
    check_raise (fun () -> nested_raise_shift log a b);
    check_raise (fun () -> nested_raise_data log a)
  ) tagged

(* TEST
   include stdlib_stable;
   modules = "test_smallint.ml";
*)

(* External declarations for unsigned comparison primitives *)
external unsigned_lt : int16 -> int16 -> bool = "%int16_unsigned_lessthan"
external unsigned_gt : int16 -> int16 -> bool = "%int16_unsigned_greaterthan"

module I = Stdlib_stable.Int16

let () =
  Test_smallint.run
    (module Stdlib_stable.Int16)
    ~min_int:(-0x8000)
    ~max_int:0x7fff;

  (* Explicit unsigned comparison tests *)

  (* Test that -1 (0xFFFF) > 0 when compared as unsigned *)
  assert (I.unsigned_compare I.minus_one I.zero = 1);
  assert (I.unsigned_compare I.zero I.minus_one = -1);

  (* Test that -32768 (0x8000) > 32767 (0x7FFF) when compared as unsigned *)
  assert (I.unsigned_compare I.min_int I.max_int = 1);
  assert (I.unsigned_compare I.max_int I.min_int = -1);

  (* Test ordering: when viewed as unsigned:
     0 < 1 < 32767 < 32768 (min_int) < 65535 (minus_one) *)
  assert (I.unsigned_compare I.zero I.one = -1);
  assert (I.unsigned_compare I.one I.max_int = -1);
  assert (I.unsigned_compare I.max_int I.min_int = -1);
  assert (I.unsigned_compare I.min_int I.minus_one = -1);

  (* Test equality *)
  assert (I.unsigned_compare I.zero I.zero = 0);
  assert (I.unsigned_compare I.minus_one I.minus_one = 0);
  assert (I.unsigned_compare I.min_int I.min_int = 0);

  (* Test specific values *)
  let neg_100 = I.of_int (-100) in (* 0xFF9C = 65436 *)
  let pos_100 = I.of_int 100 in
  assert (I.unsigned_compare neg_100 pos_100 = 1); (* 65436 > 100 *)
  assert (I.unsigned_compare pos_100 neg_100 = -1); (* 100 < 65436 *)

  (* Test mid-range values *)
  let neg_1000 = I.of_int (-1000) in (* 0xFC18 = 64536 *)
  let pos_1000 = I.of_int 1000 in
  assert (I.unsigned_compare neg_1000 pos_1000 = 1); (* 64536 > 1000 *)
  assert (I.unsigned_compare pos_1000 neg_1000 = -1); (* 1000 < 64536 *)

  (* Test the unsigned_lt primitive directly *)
  assert (unsigned_lt I.zero I.minus_one = true); (* 0 < 65535 *)
  assert (unsigned_lt I.minus_one I.zero = false); (* 65535 not < 0 *)
  assert (unsigned_lt I.max_int I.min_int = true); (* 32767 < 32768 *)
  assert (unsigned_lt I.min_int I.max_int = false); (* 32768 not < 32767 *)
  assert (unsigned_lt pos_100 neg_100 = true); (* 100 < 65436 *)
  assert (unsigned_lt neg_100 pos_100 = false); (* 65436 not < 100 *)

  (* Test unsigned greater than using primitive comparisons *)
  assert (unsigned_gt I.minus_one I.zero = true); (* 65535 > 0 *)
  assert (unsigned_gt I.zero I.minus_one = false); (* 0 not > 65535 *)
  assert (unsigned_gt I.min_int I.max_int = true); (* 32768 > 32767 *)
  assert (unsigned_gt I.max_int I.min_int = false); (* 32767 not > 32768 *)
  assert (unsigned_gt neg_100 pos_100 = true); (* 65436 > 100 *)
  assert (unsigned_gt pos_100 neg_100 = false); (* 100 not > 65436 *)

(* Tests for conversions to/from floats *)
external of_float : float -> int16 = "%int16_of_float"
external to_float : int16 -> float = "%float_of_int16"

let () =
  assert (I.equal (of_float 0.0) I.zero);
  assert (I.equal (of_float (-0.0)) I.zero);
  assert (I.equal (of_float 3.14) (I.of_int 3));
  assert (I.equal (of_float (-3.14)) (I.of_int (-3)));
  assert (I.equal (of_float (0.0 /. 0.0)) I.zero);(* strange but true *)
  assert (I.equal (of_float (1.0 /. 0.0)) I.zero);(* strange but true *)
  assert (I.equal (of_float 0.999999999999999999999999) I.one);
  assert (I.equal (of_float (-0.999999999999999999999999)) I.minus_one);
  assert (I.equal (of_float 32767.0) I.max_int);
  assert (I.equal (of_float (-32767.0)) (I.add I.min_int I.one));
  assert (I.equal (of_float (-32768.0)) I.min_int);

  assert (Float.equal (to_float I.zero) 0.0);
  assert (Float.equal (to_float I.one) 1.0);
  assert (Float.equal (to_float I.minus_one) (-1.0));
  assert (Float.equal (to_float (I.add I.one I.one)) 2.0);
  assert (Float.equal (to_float (I.sub I.minus_one I.one)) (-2.0));
  assert (Float.equal (to_float I.max_int) 32767.0);
  assert (Float.equal (to_float I.min_int) (-32768.0));

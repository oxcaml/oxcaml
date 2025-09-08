(* TEST
   include stdlib_stable;
   modules = "test_smallint.ml";
*)

(* External declarations for unsigned comparison primitives *)
external unsigned_lt : int8 -> int8 -> bool = "%int8_unsigned_lessthan"
external unsigned_gt : int8 -> int8 -> bool = "%int8_unsigned_greaterthan"

module I = Stdlib_stable.Int8

let () =
  Test_smallint.run
    (module Stdlib_stable.Int8)
    ~min_int:(-0x80)
    ~max_int:0x7f;

  (* Explicit unsigned comparison tests *)

  (* Test that -1 (0xFF) > 0 when compared as unsigned *)
  assert (I.unsigned_compare I.minus_one I.zero = 1);
  assert (I.unsigned_compare I.zero I.minus_one = -1);

  (* Test that -128 (0x80) > 127 (0x7F) when compared as unsigned *)
  assert (I.unsigned_compare I.min_int I.max_int = 1);
  assert (I.unsigned_compare I.max_int I.min_int = -1);

  (* Test ordering: when viewed as unsigned:
     0 < 1 < 127 < 128 (min_int) < 255 (minus_one) *)
  assert (I.unsigned_compare I.zero I.one = -1);
  assert (I.unsigned_compare I.one I.max_int = -1);
  assert (I.unsigned_compare I.max_int I.min_int = -1);
  assert (I.unsigned_compare I.min_int I.minus_one = -1);

  (* Test equality *)
  assert (I.unsigned_compare I.zero I.zero = 0);
  assert (I.unsigned_compare I.minus_one I.minus_one = 0);
  assert (I.unsigned_compare I.min_int I.min_int = 0);

  (* Test specific values *)
  let neg_2 = I.of_int (-2) in (* 0xFE *)
  let two = I.of_int 2 in
  assert (I.unsigned_compare neg_2 two = 1); (* 254 > 2 *)
  assert (I.unsigned_compare two neg_2 = -1); (* 2 < 254 *)

  (* Test boundary between positive and negative *)
  let neg_1 = I.of_int (-1) in (* 0xFF = 255 *)
  let pos_127 = I.of_int 127 in (* 0x7F *)
  let neg_128 = I.of_int (-128) in (* 0x80 = 128 *)
  assert (I.unsigned_compare pos_127 neg_128 = -1); (* 127 < 128 *)
  assert (I.unsigned_compare neg_128 neg_1 = -1); (* 128 < 255 *)

  (* Test the unsigned_lt primitive directly *)
  assert (unsigned_lt I.zero I.minus_one = true); (* 0 < 255 *)
  assert (unsigned_lt I.minus_one I.zero = false); (* 255 not < 0 *)
  assert (unsigned_lt I.max_int I.min_int = true); (* 127 < 128 *)
  assert (unsigned_lt I.min_int I.max_int = false); (* 128 not < 127 *)
  assert (unsigned_lt neg_2 two = false); (* 254 not < 2 *)
  assert (unsigned_lt two neg_2 = true); (* 2 < 254 *)

  (* Test unsigned greater than using primitive comparisons *)
  assert (unsigned_gt I.minus_one I.zero = true); (* 255 > 0 *)
  assert (unsigned_gt I.zero I.minus_one = false); (* 0 not > 255 *)
  assert (unsigned_gt I.min_int I.max_int = true); (* 128 > 127 *)
  assert (unsigned_gt I.max_int I.min_int = false); (* 127 not > 128 *)

(* Tests for conversions to/from floats *)
external of_float_unboxed : (float[@unboxed]) -> (int8[@untagged]) =
  "caml_int8_of_float" "caml_int8_of_float_unboxed_to_untagged"
external of_float : float -> int8 = "caml_int8_of_float"

external to_float_unboxed : (int8[@untagged]) -> (float[@unboxed]) =
  "caml_int8_to_float" "caml_int8_to_float_untagged_to_unboxed"
external to_float : int8 -> float = "caml_int8_to_float"

let () =
  assert (I.equal (of_float_unboxed 0.0) I.zero);
  assert (I.equal (of_float_unboxed (-0.0)) I.zero);
  assert (I.equal (of_float_unboxed 3.14) (I.of_int 3));
  assert (I.equal (of_float_unboxed (-3.14)) (I.of_int (-3)));
  assert (I.equal (of_float_unboxed (0.0 /. 0.0)) I.zero);(* strange but true *)
  assert (I.equal (of_float_unboxed (1.0 /. 0.0)) I.zero);(* strange but true *)
  assert (I.equal (of_float_unboxed 0.999999999999999999999999) I.one);
  assert (I.equal (of_float_unboxed (-0.999999999999999999999999)) I.minus_one);
  assert (I.equal (of_float_unboxed 127.0) I.max_int);
  assert (I.equal (of_float_unboxed (-127.0)) (I.add I.min_int I.one));
  assert (I.equal (of_float_unboxed (-128.0)) I.min_int);

  assert (I.equal (of_float 0.0) I.zero);
  assert (I.equal (of_float (-0.0)) I.zero);
  assert (I.equal (of_float 3.14) (I.of_int 3));
  assert (I.equal (of_float (-3.14)) (I.of_int (-3)));
  assert (I.equal (of_float (0.0 /. 0.0)) I.zero);(* strange but true *)
  assert (I.equal (of_float (1.0 /. 0.0)) I.zero);(* strange but true *)
  assert (I.equal (of_float 0.999999999999999999999999) I.one);
  assert (I.equal (of_float (-0.999999999999999999999999)) I.minus_one);
  assert (I.equal (of_float 127.0) I.max_int);
  assert (I.equal (of_float (-127.0)) (I.add I.min_int I.one));
  assert (I.equal (of_float (-128.0)) I.min_int);

  assert (Float.equal (to_float_unboxed I.zero) 0.0);
  assert (Float.equal (to_float_unboxed I.one) 1.0);
  assert (Float.equal (to_float_unboxed I.minus_one) (-1.0));
  assert (Float.equal (to_float_unboxed (I.add I.one I.one)) 2.0);
  assert (Float.equal (to_float_unboxed (I.sub I.minus_one I.one)) (-2.0));
  assert (Float.equal (to_float_unboxed I.max_int) 127.0);
  assert (Float.equal (to_float_unboxed I.min_int) (-128.0));

  assert (Float.equal (to_float I.zero) 0.0);
  assert (Float.equal (to_float I.one) 1.0);
  assert (Float.equal (to_float I.minus_one) (-1.0));
  assert (Float.equal (to_float (I.add I.one I.one)) 2.0);
  assert (Float.equal (to_float (I.sub I.minus_one I.one)) (-2.0));
  assert (Float.equal (to_float I.max_int) 127.0);
  assert (Float.equal (to_float I.min_int) (-128.0));

(* TEST
   include stdlib_stable;
   modules = "test_smallint.ml";
*)

(* External declarations for unsigned comparison primitives *)
external unsigned_lt : int8 -> int8 -> bool = "%int8_unsigned_lessthan"
external unsigned_gt : int8 -> int8 -> bool = "%int8_unsigned_greaterthan"

let () =
  Test_smallint.run
    (module Stdlib_stable.Int8)
    ~min_int:(-0x80)
    ~max_int:0x7f;

  (* Explicit unsigned comparison tests *)
  let module I = Stdlib_stable.Int8 in

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

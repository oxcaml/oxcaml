(* TEST
 modules = "shifted_xor_oracle.c";
 compare_programs = "false";
 arch64;
 {
   flags = "-O2";
   native;
 } {
   flags = "-O3";
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
    assert (List.rev !log = [1; 2])
  ) tagged

(* TEST
 modules = "block_checks.ml";
 flambda2;
 native;
*)

[@@@ocaml.flambda_o3]

(* Test for static allocation of unboxed array literals.

   This test verifies that constant unboxed array literals with
   [@@@ocaml.flambda_o3] are statically allocated (no heap allocation).

   The test also checks that arrays have the correct tags and headers. *)

(* Tag definitions from Cmm_helpers.Unboxed_or_untagged_array_tags *)
let unboxed_product_array_tag = 0
let unboxed_int64_array_tag = 1
let unboxed_int32_array_zero_tag = 2
let unboxed_int32_array_one_tag = 3
let unboxed_float32_array_zero_tag = 4
let unboxed_float32_array_one_tag = 5
let unboxed_vec128_array_tag = 6
let unboxed_vec256_array_tag = 7
let unboxed_vec512_array_tag = 8
let unboxed_nativeint_array_tag = 9
let untagged_int_array_tag = 10
let untagged_int16_array_zero_tag = 12
let untagged_int16_array_three_tag = 13
let untagged_int16_array_two_tag = 14
let untagged_int16_array_one_tag = 15
let untagged_int8_array_zero_tag = 16
let untagged_int8_array_seven_tag = 17
let untagged_int8_array_six_tag = 18
let untagged_int8_array_five_tag = 19
let untagged_int8_array_four_tag = 20
let untagged_int8_array_three_tag = 21
let untagged_int8_array_two_tag = 22
let untagged_int8_array_one_tag = 23

(* Helper to check allocation behavior *)
let[@inline never] check_allocation name expected_allocation f =
  Gc.full_major ();
  let words_before = Gc.minor_words () in
  let result = f () in
  let words_after = Gc.minor_words () in
  let allocated = words_after > words_before in
  if allocated <> expected_allocation then begin
    if expected_allocation then
      Printf.printf "%s: FAILED - expected allocation but none detected\n" name
    else
      Printf.printf "%s: FAILED - unexpected allocation (%.0f words)\n"
        name (words_after -. words_before)
  end else begin
    if allocated then
      Printf.printf "%s: OK - allocates as expected\n" name
    else
      Printf.printf "%s: OK - no allocation\n" name
  end;
  result

(* Test empty arrays *)
let test_empty_arrays () =
  Printf.printf "\nTesting empty arrays:\n";

  (* Empty arrays should never allocate *)
  let empty_int64 =
    check_allocation "empty int64#" false (fun () -> ([| |] : int64# array))
  in
  Block_checks.check_empty_array_is_uniform ~array_type:"empty int64#"
    (Obj.repr empty_int64);

  let empty_int32 =
    check_allocation "empty int32#" false (fun () -> ([| |] : int32# array))
  in
  Block_checks.check_empty_array_is_uniform ~array_type:"empty int32#"
    (Obj.repr empty_int32);

  let empty_int16 =
    check_allocation "empty int16#" false (fun () -> ([| |] : int16# array))
  in
  Block_checks.check_empty_array_is_uniform ~array_type:"empty int16#"
    (Obj.repr empty_int16);

  let empty_int8 =
    check_allocation "empty int8#" false (fun () -> ([| |] : int8# array))
  in
  Block_checks.check_empty_array_is_uniform ~array_type:"empty int8#"
    (Obj.repr empty_int8);

  let empty_int =
    check_allocation "empty int#" false (fun () -> ([| |] : int# array))
  in
  Block_checks.check_empty_array_is_uniform ~array_type:"empty int#"
    (Obj.repr empty_int);

  let empty_float32 =
    check_allocation "empty float32#" false (fun () -> ([| |] : float32# array))
  in
  Block_checks.check_empty_array_is_uniform ~array_type:"empty float32#"
    (Obj.repr empty_float32);

  let empty_nativeint =
    check_allocation "empty nativeint#" false
      (fun () -> ([| |] : nativeint# array))
  in
  Block_checks.check_empty_array_is_uniform ~array_type:"empty nativeint#"
    (Obj.repr empty_nativeint);

  let empty_float =
    check_allocation "empty float#" false (fun () -> ([| |] : float# array))
  in
  let tag = Obj.tag (Obj.repr empty_float) in
  assert (tag = 0);  (* float# arrays use tag 0 when empty *)

  Printf.printf "Empty array tests passed\n"

let test_array name array_type create native_tag =
  let arr =
    check_allocation name false create
  in
  let tag = Obj.tag (Obj.repr arr) in
  let expected_tag =
    match Sys.backend_type with
    | Native -> native_tag
    | Bytecode | Other _ -> 0
  in
  assert (tag = expected_tag);
  (match Sys.backend_type with
  | Native ->
      Block_checks.check_mixed_block_scannable_size
        ~array_type (Obj.repr arr) 0
  | Bytecode | Other _ -> ())

(* Test int64# arrays *)
let test_int64_arrays () =
  Printf.printf "\nTesting int64# arrays:\n";

  (* Constant arrays should be statically allocated *)
  let arr1 =
    check_allocation "int64# [42L]" false (fun () -> [: #42L :])
  in
  let tag1 = Obj.tag (Obj.repr arr1) in
  let expected_tag =
    match Sys.backend_type with
    | Native -> unboxed_int64_array_tag
    | Bytecode | Other _ -> 0
  in
  assert (tag1 = expected_tag);
  (match Sys.backend_type with
   | Native ->
       Block_checks.check_mixed_block_scannable_size
         ~array_type:"int64# single" (Obj.repr arr1) 0
   | Bytecode | Other _ -> ());

  let arr2 =
    check_allocation "int64# [1L; 2L; 3L]" false (fun () -> [: #1L; #2L; #3L :])
  in
  let tag2 = Obj.tag (Obj.repr arr2) in
  assert (tag2 = expected_tag);
  (match Sys.backend_type with
   | Native ->
       Block_checks.check_mixed_block_scannable_size
         ~array_type:"int64# triple" (Obj.repr arr2) 0
   | Bytecode | Other _ -> ());

  Printf.printf "int64# array tests passed\n"

(* Test int32# arrays *)
let test_int32_arrays () =
  Printf.printf "\nTesting int32# arrays:\n";

  let arr1 =
    check_allocation "int32# [42l]" false (fun () -> [: #42l :])
  in
  let tag1 = Obj.tag (Obj.repr arr1) in
  let expected_tag1 =
    match Sys.backend_type with
    | Native -> unboxed_int32_array_one_tag
    | Bytecode | Other _ -> 0
  in
  assert (tag1 = expected_tag1);
  (match Sys.backend_type with
   | Native ->
       Block_checks.check_mixed_block_scannable_size
         ~array_type:"int32# single" (Obj.repr arr1) 0
   | Bytecode | Other _ -> ());

  let arr2 =
    check_allocation "int32# [1l; 2l]" false (fun () -> [: #1l; #2l :])
  in
  let tag2 = Obj.tag (Obj.repr arr2) in
  let expected_tag2 =
    match Sys.backend_type with
    | Native -> unboxed_int32_array_zero_tag
    | Bytecode | Other _ -> 0
  in
  assert (tag2 = expected_tag2);
  (match Sys.backend_type with
   | Native ->
       Block_checks.check_mixed_block_scannable_size
         ~array_type:"int32# pair" (Obj.repr arr2) 0
   | Bytecode | Other _ -> ());

  let arr3 =
    check_allocation "int32# [1l; 2l; 3l]" false (fun () -> [: #1l; #2l; #3l :])
  in
  let tag3 = Obj.tag (Obj.repr arr3) in
  let expected_tag3 =
    match Sys.backend_type with
    | Native -> unboxed_int32_array_one_tag
    | Bytecode | Other _ -> 0
  in
  assert (tag3 = expected_tag3);
  (match Sys.backend_type with
   | Native ->
       Block_checks.check_mixed_block_scannable_size
         ~array_type:"int32# triple" (Obj.repr arr3) 0
   | Bytecode | Other _ -> ());

  Printf.printf "int32# array tests passed\n"

(* Test int16# arrays *)
let test_int16_arrays () =
  Printf.printf "\nTesting int16# arrays:\n";

  test_array "int16# [42S]" "int16# single"
    (fun () -> [: #42S :]) untagged_int16_array_one_tag;
  test_array "int16# [1S; 2S]" "int16# pair"
    (fun () -> [: #1S; #2S :]) untagged_int16_array_two_tag;
  test_array "int16# [1S; 2S; 3S]" "int16# triple"
    (fun () -> [: #1S; #2S; #3S :]) untagged_int16_array_three_tag;
  test_array "int16# [1S; 2S; 3S; 4S]" "int16# 4-tuple"
    (fun () -> [: #1S; #2S; #3S; #4S :]) untagged_int16_array_zero_tag;
  test_array "int16# [1S; 2S; 3S; 4S; 5S]" "int16# 5-tuple"
    (fun () -> [: #1S; #2S; #3S; #4S; #5S :]) untagged_int16_array_one_tag;

  Printf.printf "int16# array tests passed\n"

(* Test int8# arrays *)
let test_int8_arrays () =
  Printf.printf "\nTesting int8# arrays:\n";

  test_array "int8# [42s]" "int8# single"
    (fun () -> [: #42s :]) untagged_int8_array_one_tag;
  test_array "int8# [1s; 2s]" "int8# pair"
    (fun () -> [: #1s; #2s :]) untagged_int8_array_two_tag;
  test_array "int8# [1s; 2s; 3s]" "int8# triple"
    (fun () -> [: #1s; #2s; #3s :]) untagged_int8_array_three_tag;
  test_array "int8# [1s; 2s; 3s; 4s]" "int8# 4-tuple"
    (fun () -> [: #1s; #2s; #3s; #4s :]) untagged_int8_array_four_tag;
  test_array "int8# [1s; 2s; 3s; 4s; 5s]" "int8# 5-tuple"
    (fun () -> [: #1s; #2s; #3s; #4s; #5s :]) untagged_int8_array_five_tag;
  test_array "int8# [1s; 2s; 3s; 4s; 5s; 6s]" "int8# 6-tuple"
    (fun () -> [: #1s; #2s; #3s; #4s; #5s; #6s :]) untagged_int8_array_six_tag;
  test_array "int8# [1s; 2s; 3s; 4s; 5s; 6s; 7s]" "int8# 7-tuple"
    (fun () -> [: #1s; #2s; #3s; #4s; #5s; #6s; #7s :])
    untagged_int8_array_seven_tag;
  test_array "int8# [1s; 2s; 3s; 4s; 5s; 6s; 7s; 8s]" "int8# 8-tuple"
    (fun () -> [: #1s; #2s; #3s; #4s; #5s; #6s; #7s; #8s :])
    untagged_int8_array_zero_tag;
  test_array "int8# [1s; 2s; 3s; 4s; 5s; 6s; 7s; 8s; 9s]" "int8# 9-tuple"
    (fun () -> [: #1s; #2s; #3s; #4s; #5s; #6s; #7s; #8s; #9s :])
    untagged_int8_array_one_tag;

  Printf.printf "int8# array tests passed\n"

(* Test int# arrays *)
let test_int_arrays () =
  Printf.printf "\nTesting int# arrays:\n";

  test_array "int# [42m]" "int# single"
    (fun () -> [: #42m :]) untagged_int_array_tag;
  test_array "int# [1m; 2m]" "int# pair"
    (fun () -> [: #1m; #2m :]) untagged_int_array_tag;
  test_array "int# [1m; 2m; 3m]" "int# triple"
    (fun () -> [: #1m; #2m; #3m :]) untagged_int_array_tag;

  Printf.printf "int# array tests passed\n"

(* Test float32# arrays *)
let test_float32_arrays () =
  Printf.printf "\nTesting float32# arrays:\n";

  let arr1 =
    check_allocation "float32# [42.0s]" false (fun () -> [: #42.0s :])
  in
  let tag1 = Obj.tag (Obj.repr arr1) in
  let expected_tag1 =
    match Sys.backend_type with
    | Native -> unboxed_float32_array_one_tag
    | Bytecode | Other _ -> 0
  in
  assert (tag1 = expected_tag1);
  (match Sys.backend_type with
   | Native ->
       Block_checks.check_mixed_block_scannable_size
         ~array_type:"float32# single" (Obj.repr arr1) 0
   | Bytecode | Other _ -> ());

  let arr2 =
    check_allocation "float32# [1.0s; 2.0s]" false
      (fun () -> [: #1.0s; #2.0s :])
  in
  let tag2 = Obj.tag (Obj.repr arr2) in
  let expected_tag2 =
    match Sys.backend_type with
    | Native -> unboxed_float32_array_zero_tag
    | Bytecode | Other _ -> 0
  in
  assert (tag2 = expected_tag2);
  (match Sys.backend_type with
   | Native ->
       Block_checks.check_mixed_block_scannable_size
         ~array_type:"float32# pair" (Obj.repr arr2) 0
   | Bytecode | Other _ -> ());

  let arr3 =
    check_allocation "float32# [1.0s; 2.0s; 3.0s]" false
      (fun () -> [: #1.0s; #2.0s; #3.0s :])
  in
  let tag3 = Obj.tag (Obj.repr arr3) in
  let expected_tag3 =
    match Sys.backend_type with
    | Native -> unboxed_float32_array_one_tag
    | Bytecode | Other _ -> 0
  in
  assert (tag3 = expected_tag3);
  (match Sys.backend_type with
   | Native ->
       Block_checks.check_mixed_block_scannable_size
         ~array_type:"float32# triple" (Obj.repr arr3) 0
   | Bytecode | Other _ -> ());

  Printf.printf "float32# array tests passed\n"

(* Test nativeint# arrays *)
let test_nativeint_arrays () =
  Printf.printf "\nTesting nativeint# arrays:\n";

  let arr1 =
    check_allocation "nativeint# [42n]" false (fun () -> [: #42n :])
  in
  let tag1 = Obj.tag (Obj.repr arr1) in
  let expected_tag =
    match Sys.backend_type with
    | Native -> unboxed_nativeint_array_tag
    | Bytecode | Other _ -> 0
  in
  assert (tag1 = expected_tag);
  (match Sys.backend_type with
   | Native ->
       Block_checks.check_mixed_block_scannable_size
         ~array_type:"nativeint# single" (Obj.repr arr1) 0
   | Bytecode | Other _ -> ());

  let arr2 =
    check_allocation "nativeint# [1n; 2n; 3n]" false
      (fun () -> [: #1n; #2n; #3n :])
  in
  let tag2 = Obj.tag (Obj.repr arr2) in
  assert (tag2 = expected_tag);
  (match Sys.backend_type with
   | Native ->
       Block_checks.check_mixed_block_scannable_size
         ~array_type:"nativeint# triple" (Obj.repr arr2) 0
   | Bytecode | Other _ -> ());

  Printf.printf "nativeint# array tests passed\n"

(* Test float# arrays *)
let test_float_arrays () =
  Printf.printf "\nTesting float# arrays:\n";

  let arr1 =
    check_allocation "float# [42.0]" false (fun () -> [: #42.0 :])
  in
  let tag1 = Obj.tag (Obj.repr arr1) in
  assert (tag1 = 254);  (* Double_array_tag *)

  let arr2 =
    check_allocation "float# [1.0; 2.0; 3.0]" false
      (fun () -> [: #1.0; #2.0; #3.0 :])
  in
  let tag2 = Obj.tag (Obj.repr arr2) in
  assert (tag2 = 254);  (* Double_array_tag *)

  Printf.printf "float# array tests passed\n"

(* Main test *)
let () =
  Printf.printf "Testing statically-allocated unboxed arrays\n";
  Printf.printf "============================================\n";

  test_empty_arrays ();
  test_int64_arrays ();
  test_int32_arrays ();
  test_int16_arrays ();
  test_int8_arrays ();
  test_int_arrays ();
  test_float32_arrays ();
  test_nativeint_arrays ();
  test_float_arrays ();

  Printf.printf "\nAll tests passed!\n"

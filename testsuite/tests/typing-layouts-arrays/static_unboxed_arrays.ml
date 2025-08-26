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

(* Tag definitions from Cmm_helpers.Unboxed_array_tags *)
let unboxed_product_array_tag = 0
let unboxed_int64_array_tag = 1
let unboxed_int32_array_even_tag = 2
let unboxed_int32_array_odd_tag = 3
let unboxed_float32_array_even_tag = 4
let unboxed_float32_array_odd_tag = 5
let unboxed_vec128_array_tag = 6
let unboxed_vec256_array_tag = 7
let unboxed_vec512_array_tag = 8
let unboxed_nativeint_array_tag = 9

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
    | Native -> unboxed_int32_array_odd_tag
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
    | Native -> unboxed_int32_array_even_tag
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
    | Native -> unboxed_int32_array_odd_tag
    | Bytecode | Other _ -> 0
  in
  assert (tag3 = expected_tag3);
  (match Sys.backend_type with
   | Native ->
       Block_checks.check_mixed_block_scannable_size
         ~array_type:"int32# triple" (Obj.repr arr3) 0
   | Bytecode | Other _ -> ());

  Printf.printf "int32# array tests passed\n"

(* Test float32# arrays *)
let test_float32_arrays () =
  Printf.printf "\nTesting float32# arrays:\n";

  let arr1 =
    check_allocation "float32# [42.0s]" false (fun () -> [: #42.0s :])
  in
  let tag1 = Obj.tag (Obj.repr arr1) in
  let expected_tag1 =
    match Sys.backend_type with
    | Native -> unboxed_float32_array_odd_tag
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
    | Native -> unboxed_float32_array_even_tag
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
    | Native -> unboxed_float32_array_odd_tag
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
  test_float32_arrays ();
  test_nativeint_arrays ();
  test_float_arrays ();

  Printf.printf "\nAll tests passed!\n"

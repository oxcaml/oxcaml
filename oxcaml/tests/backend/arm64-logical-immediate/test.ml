(**
 * Exhaustive test of ARM64 logical immediate encoding/decoding
 *
 * This test validates the implementation of ARM64 logical immediate
 * encoding and decoding in backend/arm64/ast/logical_immediates.ml
 *
 * The test:
 * 1. Exhaustively tests all possible (n, immr, imms) field combinations
 * 2. Verifies that valid encodings can be decoded and re-encoded correctly
 * 3. Tests random values to ensure proper rejection of invalid immediates
 *
 * Files:
 * - decode.ml/decode.mli: Reference decoder implementation for validation
 * - test.ml: This file
 *
 * The test references the actual logical_immediates module from the
 * arm64_ast library (backend/arm64/ast/logical_immediates.ml) via dune.
 *)

(** Set of Nativeint values *)
module NativeintSet = Set.Make(Nativeint)

(** Test statistics *)
type stats = {
  mutable total : int;
  mutable valid : int;
  mutable invalid : int;
  mutable encoding_mismatch : int;
  mutable truly_invalid : int;
  mutable decode_errors : int;
}

let create_stats () = {
  total = 0;
  valid = 0;
  invalid = 0;
  encoding_mismatch = 0;
  truly_invalid = 0;
  decode_errors = 0;
}

let print_stats stats =
  Printf.printf "=== Test Results ===\n";
  Printf.printf "Total combinations tested: %d\n" stats.total;
  Printf.printf "Valid encodings: %d\n" stats.valid;
  Printf.printf "Invalid encodings (all 0s): %d\n" stats.invalid;
  Printf.printf "Truly invalid encodings (reserved/all 1s): %d\n" stats.truly_invalid;
  Printf.printf "Encoding mismatches: %d\n" stats.encoding_mismatch;
  Printf.printf "Unexpected decode errors: %d\n" stats.decode_errors;
  Printf.printf "====================\n"

(** Test a single (n, immr, imms) combination *)
let test_combination stats n immr imms =
  stats.total <- stats.total + 1;

  try
    (* Decode the fields to get a 64-bit value *)
    let decoded = Decode.decode_logical_immediate ~n ~immr ~imms in

    (* Check if this value is recognized as a valid logical immediate *)
    let is_valid = Arm64_ast.Logical_immediates.is_logical_immediate decoded in

    if is_valid then begin
      stats.valid <- stats.valid + 1;

      (* Try to encode it back *)
      try
        let (n', immr', imms') = Arm64_ast.Logical_immediates.encode_logical_immediate_fields decoded in

        (* Check if we get back the same encoding *)
        (* Note: There might be multiple valid encodings for the same value *)
        if n <> n' || immr <> immr' || imms <> imms' then begin
          (* Verify that both encodings produce the same value *)
          let decoded' = Decode.decode_logical_immediate ~n:n' ~immr:immr' ~imms:imms' in
          if decoded <> decoded' then begin
            stats.encoding_mismatch <- stats.encoding_mismatch + 1;
            Printf.printf "MISMATCH: n=%d immr=%d imms=%d -> 0x%Lx -> n=%d immr=%d imms=%d -> 0x%Lx\n"
              n immr imms (Int64.of_nativeint decoded)
              n' immr' imms' (Int64.of_nativeint decoded')
          end
        end
      with Invalid_argument msg ->
        (* The decoded value couldn't be encoded back - this is a problem *)
        stats.encoding_mismatch <- stats.encoding_mismatch + 1;
        Printf.printf "ERROR: n=%d immr=%d imms=%d decoded to 0x%Lx but can't be encoded: %s\n"
          n immr imms (Int64.of_nativeint decoded) msg
    end else begin
      (* Invalid encoding (produces all 0s) *)
      (* Note: all-1s patterns now throw exceptions in decode, so we only see 0s here *)
      assert (decoded = 0n);
      stats.invalid <- stats.invalid + 1
    end;
    Some decoded
  with
  | Invalid_argument msg when
      String.starts_with ~prefix:"decode_logical_immediate:" msg ->
    (* Expected invalid encoding (reserved or all-1s pattern) *)
    stats.truly_invalid <- stats.truly_invalid + 1;
    None
  | e ->
    (* Unexpected error during decoding *)
    stats.decode_errors <- stats.decode_errors + 1;
    Printf.printf "UNEXPECTED ERROR: n=%d immr=%d imms=%d: %s\n"
      n immr imms (Printexc.to_string e);
    None

(** Run exhaustive test over all possible encodings *)
let run_exhaustive_test () =
  let stats = create_stats () in
  let seen_values = ref NativeintSet.empty in

  Printf.printf "Starting exhaustive test of ARM64 logical immediate encodings...\n";
  Printf.printf "Testing all combinations: n=[0,1], immr=[0,63], imms=[0,63]\n\n";

  (* Iterate over all possible values *)
  for n = 0 to 1 do
    for immr = 0 to 63 do
      for imms = 0 to 63 do
        match test_combination stats n immr imms with
        | Some decoded -> seen_values := NativeintSet.add decoded !seen_values
        | None -> ()
      done
    done
  done;

  print_stats stats;

  (* Exit with error code if there were any issues *)
  if stats.encoding_mismatch > 0 || stats.decode_errors > 0 then
    exit 1;

  !seen_values

(** Generate a random 64-bit nativeint *)
let random_nativeint () =
  let high = Random.bits () in
  let low = Random.bits () in
  Nativeint.(logor (shift_left (of_int high) 32) (of_int low))

(** Test random values that aren't valid logical immediates *)
let test_random_values seen_values num_tests =
  Printf.printf "\nTesting random values (should be rejected if not seen)...\n";
  Printf.printf "Testing %d random values\n\n" num_tests;

  let false_positives = ref 0 in

  for _i = 1 to num_tests do
    let value = random_nativeint () in
    if not (NativeintSet.mem value seen_values) then begin
      let is_valid = Arm64_ast.Logical_immediates.is_logical_immediate value in
      if is_valid then begin
        incr false_positives;
        Printf.printf "FALSE POSITIVE: 0x%Lx reported as valid but not in encoding set\n"
          (Int64.of_nativeint value)
      end
    end
  done;

  Printf.printf "=== Random Test Results ===\n";
  Printf.printf "False positives: %d\n" !false_positives;
  Printf.printf "===========================\n";

  if !false_positives > 0 then
    exit 1

let () =
  Random.self_init ();
  let seen_values = run_exhaustive_test () in
  Printf.printf "\nUnique values in encoding set: %d\n" (NativeintSet.cardinal seen_values);
  (* Test with 10M random values. Takes approximately 4 seconds to complete. *)
  test_random_values seen_values 10_000_000

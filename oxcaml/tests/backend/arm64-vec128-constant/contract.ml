(* The ordinary conversion must preserve OCaml's 63-bit wrapping arithmetic.
   This program performs no memory access with an invalid index. *)
let[@inline never] wrapped_word o k = Int64.of_int (o + k)

let tagged_model o k =
  let tag = Int64.logor (Int64.shift_left (Int64.of_int o) 1) 1L in
  Int64.shift_right (Int64.add tag (Int64.shift_left (Int64.of_int k) 1)) 1

let distributed_model o k = Int64.add (Int64.of_int o) (Int64.of_int k)

let () =
  assert (Sys.int_size = 63);
  let cases =
    Sys.opaque_identity
      [ min_int, min_int;
        min_int, -16;
        min_int, 16;
        min_int + 15, -16;
        -16, 16;
        -1, 16;
        0, 16;
        1, 16;
        max_int - 16, 16;
        max_int - 15, 16;
        max_int - 7, 16;
        max_int, 16;
        max_int, -16 ]
  in
  Printf.printf "offset,delta,wrapped63,tagged_model,distributed64,equal\n";
  List.iter
    (fun (o, k) ->
      let wrapped = wrapped_word o k in
      let tagged = tagged_model o k in
      let distributed = distributed_model o k in
      assert (wrapped = tagged);
      Printf.printf "%d,%d,%Ld,%Ld,%Ld,%b\n" o k wrapped tagged distributed
        (wrapped = distributed))
    cases;
  assert (wrapped_word (max_int - 15) 16 = -4611686018427387904L);
  assert (distributed_model (max_int - 15) 16 = 4611686018427387904L);
  (* A negative constant can wrap all the way to a valid index. *)
  assert (wrapped_word min_int min_int = 0L);
  assert (distributed_model min_int min_int = Int64.min_int)

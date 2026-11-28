(* TEST
 include stdlib_stable;
 {
   reference = "${test_source_directory}/test_iarray.byte.reference";
   bytecode;
 }{
   stack-allocation;
   reference = "${test_source_directory}/test_iarray.stack.reference";
   native;
 }{
   no-stack-allocation;
   reference = "${test_source_directory}/test_iarray.heap.reference";
   native;
 }
*)

(* Testing all the [iarray] functions that allocate [iarray]s locally (not
   including multiple variants of the same function) for:

   1. Safety: No forward pointers on the local stack.

   2. Correctness: They actually create arrays on the stack (by testing that no
      GCed allocation happens). *)

module Iarray = Stdlib_stable.IarrayLabels

external opaque_local : 'a @ local -> 'a @ local = "%opaque"

let ignore_local : 'a @ local -> unit = fun x ->
  let _ @ local = opaque_local x in
  ()

let local_some : 'a -> 'a option @ local = fun x -> exclave_ Some x

let run name f x = exclave_
  let prebefore = Gc.allocated_bytes () in
  let before = Gc.allocated_bytes () in
  let r = Sys.opaque_identity f x in
  let after = Gc.allocated_bytes () in
  let delta =
    int_of_float ((after -. before) -. (before -. prebefore))
      / (Sys.word_size/8)
  in
  let msg =
    match delta with
    | 0 -> "No Allocation"
    | n -> "Allocation"
  in
  Printf.printf "%20s: %s\n" name msg;
  r

(* Testing functions *)

let test_init n = exclave_ Iarray.init_local n ~f:local_some

let test_append (a1, a2) = exclave_ Iarray.append_local a1 a2

let test_concat = Iarray.concat_local

let test_sub a = exclave_ Iarray.sub_local a ~pos:0 ~len:3

let test_to_list = Iarray.to_list_local

let test_of_list = Iarray.of_list_local

let test_map a = exclave_
  Iarray.map_local
    ~f:(function Some x -> exclave_ Some (-x) | None -> exclave_ Some 0)
    a

let test_mapi a = exclave_ Iarray.mapi_local ~f:(fun i x -> exclave_ i, x) a

let test_fold_left a = exclave_
  Iarray.fold_left_local ~f:(fun l x -> exclave_ x :: l) ~init:[] a

let test_fold_left_map a = exclave_
  Iarray.fold_left_map_local ~f:(fun l x -> exclave_ x :: l, Some x) ~init:[] a

let test_fold_right a = exclave_
  Iarray.fold_right_local ~f:(fun x l -> exclave_ x :: l) a ~init:[]

let test_map2 (a1, a2) = exclave_ Iarray.map2_local ~f:(fun x y -> exclave_ x, y) a1 a2

let test_split = Iarray.split_local

let test_combine (a1, a2) = exclave_ Iarray.combine_local a1 a2

(* Run the test, keeping the values alive *)
let () =
  let r0 @ local = run "init_local"
    test_init
    42
  in
  let r1 @ local = run "append_local"
    test_append
    (Iarray.init_local 42 ~f:local_some, [:None; Some (-1); Some (-2):])
  in
  let r2 @ local = run "concat_local"
    test_concat
    [Iarray.init_local 42 ~f:local_some; [:None; Some (-1); Some (-2):]]
  in
  let r3 @ local = run "sub_local"
    test_sub
    (Iarray.init_local 42 ~f:local_some)
  in
  let r4 @ local = run "to_list_local"
    test_to_list (Iarray.init_local 42 ~f:local_some)
  in
  let r5 @ local = run "of_list_local"
    test_of_list [Some 0; Some 1; Some 2; Some 3; Some 4; Some 5]
  in
  let r6 @ local = run "map_local"
    test_map
    (Iarray.init_local 42 ~f:local_some)
  in
  let r7 @ local = run "mapi_local"
    test_mapi
    (Iarray.init_local 42 ~f:local_some)
  in
  let r8 @ local = run "fold_left_local"
    test_fold_left
    (Iarray.init_local 42 ~f:local_some)
  in
  let r9 @ local = run "fold_left_map_local"
    test_fold_left_map
    (Iarray.init_local 42 ~f:local_some)
  in
  let rA @ local = run "fold_right_local"
    test_fold_right
    (Iarray.init_local 42 ~f:local_some)
  in
  let rB @ local = run "map2_local"
    test_map2
    (Iarray.init_local 3 ~f:local_some, [:None; Some (-1); Some (-2):])
  in
  let rC @ local = run "split_local"
    test_split
    (Iarray.init_local 42 ~f:(fun i -> exclave_ Some i, Some (-i)))
  in
  let rD @ local = run "combine_local"
    test_combine
    (Iarray.init_local 3 ~f:local_some, [:None; Some (-1); Some (-2):])
  in
  (* In debug mode, Gc.minor () checks for backwards local pointers and minor
     heap->local pointers (though we're more concerned about the former) *)
  Gc.minor ();
  ignore_local (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, rA, rB, rC, rD);
  ()

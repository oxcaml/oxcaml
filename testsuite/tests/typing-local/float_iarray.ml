(* TEST
 include stdlib_stable;
 {
   flat-float-array;
   {
     reference = "${test_source_directory}/float_iarray.heap.reference";
     bytecode;
   }{
     stack-allocation;
     reference = "${test_source_directory}/float_iarray.stack.reference";
     native;
   }{
     no-stack-allocation;
     reference = "${test_source_directory}/float_iarray.heap.reference";
     native;
   }
 }{
   no-flat-float-array;
   reference = "${test_source_directory}/float_iarray.heap.no-flat.reference";
   {
     bytecode;
   }{
     native;
   }
 }
*)


(* Testing that local [float iarray]s don't allocate on access.  This is a
   question because for flat float arrays, accesses have to box the float. *)

module Iarray = Stdlib_stable.IarrayLabels

let ( .:() ) = Iarray.( .:() )

external opaque_local : 'a @ local -> 'a @ local = "%opaque"

let ignore_local : 'a @ local -> unit = fun x ->
  let _ @ local = opaque_local x in
  ()

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
  Printf.printf "%23s: %s\n" name msg;
  r

(* Testing functions *)

let test_access : float iarray @ local -> float @ local =
  fun iarr -> exclave_ iarr.:(0)

let test_match : float iarray @ local -> float @ local =
  fun iarr ->
  match iarr with
  | [: _; two; _ :] -> two
  | _ -> assert false

(* Run the test, keeping the values alive *)
let () =
  let r0 @ local = run "access from literal"
    test_access
    [: 2.7; 3.1; 1.0 :]
  in
  let r1 @ local = run "access from Iarray.init"
    test_access
    (Iarray.init_local 10 ~f:(fun i -> Float.of_int i))
  in
  (* TODO: Matching currently allocates, but that should be fixed eventually *)
  let r2 @ local = run "match on literal"
    test_match
    [: 2.7; 3.1; 1.0 :]
  in
  let r3 @ local = run "match on Iarray.init"
    test_match
    (Iarray.init_local 3 ~f:(fun i -> Float.of_int i))
  in
  ignore_local (r0, r1, r2, r3);
  ()

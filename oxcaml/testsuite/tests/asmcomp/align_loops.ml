(* TEST
 arch_amd64;
 {
   flags = "-S -align-loops";
   reference = "${test_source_directory}/align_loops.reference";
   native;
 }{
   flags = "-S";
   reference = "${test_source_directory}/align_loops.off.reference";
   native;
 }
*)

(* Check that -align-loops emits a 16-byte alignment directive before
   loop header labels (see align_loops.run). *)

let[@inline never] sum_to n =
  let total = ref 0 in
  for i = 1 to n do
    total := !total + i
  done;
  !total

(* A self tail call is compiled to a loop. *)
let[@inline never] rec count_down acc n =
  if n = 0 then acc else count_down (acc + n) (n - 1)

let () =
  assert (sum_to 10 = 55);
  assert (count_down 0 10 = 55)

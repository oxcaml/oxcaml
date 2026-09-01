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
   labels that are targets of backward branches (see align_loops.run). *)

let[@inline never] sum_to n =
  let total = ref 0 in
  for i = 1 to n do
    total := !total + i
  done;
  !total

let () = assert (sum_to 10 = 55)

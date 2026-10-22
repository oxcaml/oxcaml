(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=db,nostamps";
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt;
   script = "sh ${test_source_directory}/check-escaping-flow.sh ${compiler_output2}";
   script;
   run;
   check-program-output;
 *)

(* Since [id] is exported, both pairs have unknown usage before escape
   analysis starts. Do not propagate their candidate sources through [id]: N
   such callers otherwise produce N squared facts, all for escaping values.
   Check the database rather than using a machine-dependent time limit. *)
let[@inline never] id (p : int * int) = p

let[@inline never] sum_pair (p : int * int) = fst p + snd p

let[@inline never] f1 x =
  let p = id (x, 1) in
  sum_pair p

let[@inline never] f2 x =
  let p = id (x, 2) in
  sum_pair p

let () = (Printf.printf [@inlined never]) "%d %d\n" (f1 10) (f2 20)

(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_loop_early_return";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_loop_early_return] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p
let[@inline never] find n =
  let rec loop i prev =
    if i > n then -1
    else
      let p = (i, i * i) in
      if sum_pair p + sum_pair prev > 50 then i else loop (i + 1) p
  in
  loop 1 (0, 0)
let[@inline never] count n =
  let rec loop i acc =
    if i > n then acc
    else
      let local_ p = (i, i) in
      if sum_pair p > 1000 then acc else loop (i + 1) (acc + sum_pair p)
  in
  loop 1 0
let () = (Printf.printf [@inlined never]) "%d %d\n" (find 100) (count 100)

(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_pair_to_callee";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_pair_to_callee] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

let[@inline never] f_tail x y =
  let p = (x, y) in
  sum_pair p

let[@inline never] f x y =
  let local_ p = (x, y) in
  (sum_pair p [@nontail])

let () = (Printf.printf [@inlined never]) "%d %d\n" (f_tail 3 4) (f 3 4)

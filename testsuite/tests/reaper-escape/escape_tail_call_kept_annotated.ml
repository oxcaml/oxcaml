(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_tail_call_kept";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_tail_call_kept] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p
let[@inline never] g s = s * 2
let[@inline never] f x y =
  let local_ p = (x, y) in
  let s = sum_pair p in
  g s
let () = (Printf.printf [@inlined never]) "%d\n" (f 3 4)

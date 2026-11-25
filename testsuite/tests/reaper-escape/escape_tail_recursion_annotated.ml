(* TEST
   flambda2;
   stack-allocation;
   unset OCAMLPARAM;
   flags += "-flambda2-reaper -no-experimental-reaper-escape -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_tail_recursion";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_tail_recursion] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

let rec ping n acc =
  if n = 0 then acc else let local_ p = (n, 1) in pong (n - 1) (acc + sum_pair p)

and pong n acc =
  if n = 0 then acc else let local_ p = (n, 2) in ping (n - 1) (acc + sum_pair p)

let () = (Printf.printf [@inlined never]) "%d\n" (ping 10_000_000 0)

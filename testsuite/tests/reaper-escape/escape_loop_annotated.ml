(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_loop";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_loop] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

let[@inline never] f n =
  let acc = ref 0 in
  for i = 1 to n do
    let local_ p = (i, i + 1) in
    acc := !acc + sum_pair p
  done;
  !acc

let () = (Printf.printf [@inlined never]) "%d\n" (f 10)

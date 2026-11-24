(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_loop_try";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_loop_try] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p
let[@inline never] boom (p : int * int) =
  if fst p mod 3 = 0 then raise Exit else snd p
let[@inline never] f n =
  let acc = ref 0 in
  for i = 1 to n do
    let p = (i, i) in
    (try acc := !acc + boom p with Exit -> acc := !acc + sum_pair p)
  done;
  (try
     for i = 1 to n do
       let p = (i, i) in
       acc := !acc + boom p
     done
   with Exit -> ());
  !acc
let () = (Printf.printf [@inlined never]) "%d\n" (f 10)

(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_loop_exception_regions";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_loop_exception_regions] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
external local_stack_offset : unit -> int = "caml_local_stack_offset"

let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

let[@inline never] boom (local_ (p : int * int)) =
  let local_ q = fst p + 1, snd p + 2 in
  let value = sum_pair q in
  if fst p mod 3 = 0 then raise Exit else value

let[@inline never] inside n =
  let acc @ global = ref 0 in
  for i = 1 to n do
    let local_ p = i, i in
    try acc := !acc + boom p with Exit ->
      let local_ q = i + 10, i + 20 in
      acc := !acc + sum_pair p + sum_pair q
  done;
  !acc

let[@inline never] outside n =
  let acc @ global = ref 0 in
  (try
     for i = 1 to n do
       let p = i, i in
       acc := !acc + boom p
     done
   with Exit -> ());
  !acc

let () =
  let before = local_stack_offset () in
  let a = inside 10 in
  let b = outside 10 in
  let after = local_stack_offset () in
  (Printf.printf [@inlined never]) "%d %d; stack growth %d\n"
    a b (after - before)

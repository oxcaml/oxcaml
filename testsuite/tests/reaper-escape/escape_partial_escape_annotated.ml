(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_partial_escape";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_partial_escape] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] f x y z =
  let inner = (x, y) in
  let outer = (inner, z) in
  outer

let () =
  let (a, b), c = f 1 2 3 in
  (Printf.printf [@inlined never]) "%d %d %d\n" a b c

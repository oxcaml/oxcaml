(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_nested_blocks";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_nested_blocks] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] sum_nested (local_ (p : (int * int) * int)) =
  let (a, b), c = p in
  a + b + c

let[@inline never] f x y z =
  let local_ inner = (x, y) in
  let local_ outer = (inner, z) in
  (sum_nested outer [@nontail])

let () = (Printf.printf [@inlined never]) "%d\n" (f 1 2 3)

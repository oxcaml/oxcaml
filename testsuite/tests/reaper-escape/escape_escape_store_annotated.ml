(* TEST
   flambda2;
   stack-allocation;
   unset OCAMLPARAM;
   flags += "-flambda2-reaper -no-experimental-reaper-escape -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_escape_store";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_escape_store] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let cell = ref (0, 0)

let[@inline never] f x y =
  let p = (x, y) in
  cell := p

let () =
  f 5 6;
  let a, b = !cell in
  (Printf.printf [@inlined never]) "%d %d\n" a b

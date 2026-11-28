(* TEST
   flambda2;
   stack-allocation;
   unset OCAMLPARAM;
   flags += "-flambda2-reaper -no-experimental-reaper-escape -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_closure_local";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_closure_local] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] apply_twice (local_ (f : int -> int)) x = f (f x)

let[@inline never] g a b =
  let local_ h y = (y * a) + b in
  (apply_twice h 1 [@nontail])

let () = (Printf.printf [@inlined never]) "%d\n" (g 2 3)

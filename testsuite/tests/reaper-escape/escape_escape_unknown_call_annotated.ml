(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_escape_unknown_call";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_escape_unknown_call] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). *)
let[@inline never] f x y =
  let p = (x, y) in
  Hashtbl.hash p

let () = (Printf.printf [@inlined never]) "%d\n" (f 1 2 land 0xff)

(* TEST
   modules = "specialise_lifted_stale_hint_lib.ml";
   flambda2;
   setup-ocamlopt.opt-build-env;
   all_modules = "specialise_lifted_stale_hint.ml";
   binary_modules = "specialise_lifted_stale_hint_lib";
   {
     flags = "-O4 -flambda2-reaper -reaper-local-fields -dflambda-invariants";
     flags += " -flambda2-inline-large-function-size 100";
     module = "specialise_lifted_stale_hint_lib.ml";
     ocamlopt.opt;
     compiler_output2 = "consumer.cmm";
     flags = "-O3 -no-flambda2-reaper -dflambda-invariants -dcmm";
     flags += " -flambda2-inline-small-function-size 0";
     flags += " -flambda2-inline-large-function-size 100";
     flags += " -flambda2-inline-threshold 10";
     module = "";
     ocamlopt.opt;
     script = "sh ${test_source_directory}/check-stale-hint-decision.sh consumer.cmm";
     script;
     run;
     check-program-output;
   }{
     flags = "-O4 -flambda2-reaper -reaper-local-fields -dflambda-invariants";
     flags += " -flambda2-inline-small-function-size 0";
     flags += " -flambda2-inline-large-function-size 100";
     module = "specialise_lifted_stale_hint_lib.ml";
     ocamlopt.opt;
     compiler_output2 = "consumer.cmm";
     flags = "-O3 -no-flambda2-reaper -dflambda-invariants -dcmm";
     flags += " -flambda2-inline-small-function-size 0";
     flags += " -flambda2-inline-large-function-size 100";
     flags += " -flambda2-inline-threshold 10";
     module = "";
     ocamlopt.opt;
     script = "sh ${test_source_directory}/check-stale-hint-decision.sh consumer.cmm";
     script;
     run;
     check-program-output;
   }
 *)

(* Stale hints must not retain [captured1]'s allocation during speculation:
   [outer_hint] and [outer_control] should both inline. Conversely, newly
   specialised code must be charged: [large] must not be copied with an unknown
   callback, even when its producer classified the wrapper as small. The two
   builds exercise the automatic-small and ordinary speculative paths. *)

let f_hint x = Specialise_lifted_stale_hint_lib.outer_hint x true
let f_control x = Specialise_lifted_stale_hint_lib.outer_control x true
let[@inline never] f_large f n = Specialise_lifted_stale_hint_lib.large f n

let () =
  List.iter
    (fun x ->
      Printf.printf "%d: %d %d; %d\n" x (f_hint x) (f_control x)
        (f_large (fun y -> y + 1) x))
    [0; 7]

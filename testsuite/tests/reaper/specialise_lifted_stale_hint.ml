(* TEST
   modules = "specialise_lifted_stale_hint_lib.ml";
   flambda2;
   setup-ocamlopt.opt-build-env;
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
   all_modules = "specialise_lifted_stale_hint.ml";
   binary_modules = "specialise_lifted_stale_hint_lib";
   ocamlopt.opt;
   script = "sh ${test_source_directory}/check-stale-hint-decision.sh";
   script += " consumer.cmm";
   script;
   run;
   check-program-output;
 *)

(* Only the producer runs the reaper. It lifts [g1] and [g2] into one site with
   slots for both captures, although lifted [g2] takes only [captured2].
   Inlining [outer_hint _ true] removes the else branch and unboxes the unread
   [r]; the call to [g2] keeps the site. Since [names_available_for_hints] is
   computed before mutable unboxing, the [captured1] slot keeps its dead
   allocation in the speculative body. Its cost prevents inlining: the Cmm
   keeps the call to [outer_hint], but [outer_control], which differs only by
   [ref captured2], is inlined. Known limitation: the extra simplification pass
   repairs the non-speculative case and To_cmm drops the dead allocation, so
   only the inlining decision is affected. *)

let f_hint x = Specialise_lifted_stale_hint_lib.outer_hint x true
let f_control x = Specialise_lifted_stale_hint_lib.outer_control x true

let () =
  List.iter
    (fun x -> Printf.printf "%d: %d %d\n" x (f_hint x) (f_control x))
    [0; 7]

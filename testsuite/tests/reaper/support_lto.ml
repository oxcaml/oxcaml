(* TEST
   flambda2;
   compile_only = "true";
   flags += "-flambda2-reaper -reaper-local-fields -reaper-debug-flags=nostamps";
   setup-ocamlopt.opt-build-env;
   {
     ocamlopt.opt;
     script = "cp support_lto.o support_lto.no_lto.o";
     script;
   }{
     flags += " -support-lto -reaper-debug-flags=confirm-run";
     ocamlopt.opt;
     check-ocamlopt.opt-output;
     script = "cmp support_lto.o support_lto.no_lto.o";
     script;
   }
 *)

(* This test checks that running the reaper twice (as happens under
   [-support-lto]) does not change the generated code: the object file is
   identical with and without the flag, so the second run finds nothing the
   first missed. (The textual IR can differ cosmetically - a second run
   re-prefixes some value-slot names - but the compiled code is unchanged, so we
   compare object files rather than the reaper IR dump.)

   The first config compiles without the flag (one reaper run) and saves the
   object file; the second config passes [-support-lto] (two reaper runs) and
   compares its object file against the saved one with [cmp].

   The second config also enables [-reaper-debug-flags=confirm-run], which makes
   each reaper run print "reaper: running". [check-ocamlopt.opt-output] confirms
   this line appears twice, proving the reaper genuinely ran a second time. *)

let f x =
  let g y =
    let[@inline never][@local never] h u = u in
    let (a, _) = h (x, y) in
    a
  in
  let () = () in
  g

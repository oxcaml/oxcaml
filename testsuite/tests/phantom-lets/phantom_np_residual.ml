(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, the reaper, or dump-dir redirection), which would
    perturb the Cmm dump this test greps. *)
 set OCAMLPARAM = "_,";
 setup-ocamlopt.byte-build-env;
 {
   flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dcmm -dump-into-file";
   module = "phantom_np_residual.ml";
   ocamlopt.byte;
 }{
   script = "sh ${test_source_directory}/check-contains.sh phantom_np_residual.cmx.dump let? name_for_debugger";
   script;
 }
*)

(* Phantom lets referencing non-user-visible temporaries, distilled from
   the testsuite's raw_backtrace.ml.

   After inlining, the components of the [Some (exn, trace)] pair are
   projected via compiler-generated [Pfield] temporaries; Simplify deletes
   the (inlined-body) parameter bindings, leaving phantom lets whose
   defining expressions reference those temporaries, which are marked
   "needed by phantom let" (see pr15b).  The temporary holding the [trace]
   component is sunk past the flush that would emit the phantom let and its
   binding is then inlined into the argument of an extcall inside a
   continuation handler, so the referenced binder is not in scope where the
   phantom let would be emitted.  Since that reference is the phantom let's
   only one, the phantom let must not be emitted there at all (see
   [To_cmm_env.flush_delayed_lets]) -- rather than causing a fatal error
   ("Unbound free_vars in function body when translating to cmm").  A copy
   of it is instead emitted next to the extcall, beneath the temporary's
   (empty) phantom binder; the phantom let for the [exn] component, whose
   referenced binder is in scope, keeps its defining expression where it
   is.

   Every ingredient below is load-bearing: the array access, the pair
   inside the option, and both [Printexc] calls (whose inlined bodies
   supply the phantom lets). *)

[@@@ocaml.warning "-26-27-32"]

exception Error of string

let[@inline never] [@local never] g msg = raise (Error msg)

let backtrace args =
  try
    ignore (g args.(0)); None
  with exn ->
    let exn = Printexc.to_string exn in
    let trace = Printexc.get_raw_backtrace () in
    Some (exn, trace)

let[@inline never] run args =
  match backtrace args with
    | None -> print_string "No exception\n"
    | Some (exn, trace) ->
      print_string exn;
      Printexc.print_raw_backtrace stdout trace

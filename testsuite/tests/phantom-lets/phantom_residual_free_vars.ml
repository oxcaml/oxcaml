(* TEST
 flambda2;
 setup-ocamlopt.byte-build-env;
 {
   flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dcmm -dump-into-file";
   module = "phantom_residual_free_vars.ml";
   ocamlopt.byte;
 }{
   script = "sh ${test_source_directory}/check-contains.sh phantom_residual_free_vars.cmx.dump let?";
   script;
 }
*)

(* Phantom-mode free variables that remain at the boundary of a function
   body must be bound as empty phantom lets rather than causing a fatal
   error ("Unbound free_vars in function body when translating to cmm").

   This test is a distillation of [unsafe_really_input] in the stdlib,
   which is where the problem was first seen.  What happens, step by step
   (using the parameter [ic] as the example; [buf] behaves identically):

   1. At -O3 the self tail call is loopified, and the loop handler begins
      with alias lets rebinding the function's parameters: essentially
      [let ic' = ic in ...].  In [To_cmm], such a binding is pure and its
      Cmm form is just a variable, so it is classified
      [Must_inline_and_duplicate]: it is kept in the delayed-bindings
      environment so that every use, in any branch, can be substituted.

   2. When [ic'] is substituted into the arguments of the [input_stub]
      call, it is user visible and phantom lets are enabled, so the
      substitution is wrapped: [Cname_for_debugger (ic', Cvar ic)] -- and
      [ic'] is recorded as a phantom-mode free variable.  That record is a
      promissory note: some enclosing flush of the delayed bindings must
      emit an empty phantom let for [ic'] so that the naming operation has
      a binder for the debugger.

   3. The call is effectful, so it is not emitted inline either: it
      becomes another delayed binding, and the promissory note is stored
      inside that binding's record, invisible to any flush until the call
      binding is itself flushed.

   4. Each leaf of the function (the return, the raise, the loop back
      edge) flushes the environment for its own branch.  In branches where
      [ic'] has no occurrences, the flush sees a pure binding with no uses
      and drops it, which is correct for that branch.

   5. In the branch that does use [ic'], the flush that emits the call
      finally surfaces the phantom-mode free variable -- but that flush is
      a branching-point flush, which deliberately keeps
      [Must_inline_and_duplicate] bindings so that later code can still
      substitute them.  So the binding is not turned into a phantom let
      there either; when the handler is finished, the environment holding
      it is simply discarded.

   6. Net effect: the [Cname_for_debugger] is embedded in the code, the
      phantom-mode free variable propagates to the function boundary, and
      no phantom let was ever emitted for it.

   The discharge logic assumes that some flush will see the binding and
   the phantom occurrence together; the combination of "occurrence hidden
   inside another delayed binding" and "binding kept for duplication, then
   dropped branch-locally" breaks that assumption.  The fix honours the
   promissory note at the function boundary: residual phantom-mode free
   variables are bound there as empty phantom lets (visible as [let?] with
   no defining expression in the -dcmm output, and checked for below), so
   the variable is presented to the debugger as optimised out except where
   a naming operation provides availability.  Residual free variables at
   normal mode still indicate a genuine translation bug and remain
   fatal. *)

external input_stub : in_channel -> bytes -> int -> int -> int
  = "caml_ml_input"

let rec unsafe_really_input ic buf ofs len =
  if len <= 0
  then ()
  else begin
    let r = input_stub ic buf ofs len in
    if r = 0
    then raise End_of_file
    else unsafe_really_input ic buf (ofs + r) (len - r)
  end

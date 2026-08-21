(* TEST
 flambda2;
 (* Insulate the test from OCAMLPARAM settings used by CI configurations
    (e.g. classic mode, the reaper, or dump-dir redirection), which would
    perturb the Cmm dump this test greps. *)
 set OCAMLPARAM = "_,";
 setup-ocamlopt.byte-build-env;
 {
   flags = "-O3 -g -gno-upstream-dwarf -flambda2-expert-phantom-lets -dcmm -dump-into-file";
   module = "phantom_residual_free_vars.ml";
   ocamlopt.byte;
 }{
   script = "sh ${test_source_directory}/check-contains.sh phantom_residual_free_vars.cmx.dump name_for_debugger";
   script;
 }
*)

(* This test is a distillation of [unsafe_really_input] in the stdlib.

   At -O3 the self tail call is loopified, and the loop handler begins
   with alias lets rebinding the function's parameters: essentially
   [let ic' = ic in ...].  In [To_cmm], such a binding is pure and its
   Cmm form is just a variable, so it is classified
   [Must_inline_and_duplicate]: it is kept in the delayed-bindings
   environment so that every use, in any branch, can be substituted.
   When [ic'] is substituted into the arguments of the (effectful, hence
   itself delayed) [input_stub] call, it is user visible and phantom lets
   are enabled, so the substitution is wrapped:
   [Cname_for_debugger (ic', Cvar ic)].

   The wrapper is an annotation only ("this value used to be called
   [ic']"), from which instruction selection produces a naming operation;
   the variable it names does not count as a free variable and requires
   no binding.  An earlier version of this code recorded [ic'] as a
   phantom-mode free variable, to be discharged by an empty phantom let
   at flush time; but the occurrence sits hidden inside the delayed call
   binding while the alias binding is kept for duplication and then
   dropped branch-locally, so no single flush saw the two together and
   the variable reached the function boundary unbound, causing a fatal
   error ("Unbound free_vars in function body when translating to cmm").

   This test checks that the pattern compiles and that the naming
   wrappers for the rebound parameters are present in the Cmm output. *)

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

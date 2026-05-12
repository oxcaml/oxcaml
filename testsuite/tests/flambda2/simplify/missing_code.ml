(* TEST
   compile_only = "true";
   flambda2;
   readonly_files = "missing_code_step1.ml missing_code_step2.ml";
   setup-ocamlopt.byte-build-env;
   { all_modules = "missing_code_step1.ml";
     ocamlopt_flags += " -flambda2-result-types-functors-only";
     ocamlopt.byte;
   }{
     all_modules = "missing_code_step2.ml";
     ocamlopt.byte;
   }{
     script = "rm missing_code_step1.cmx";
     script;
   }{
     all_modules = "missing_code.ml";
     ocamlopt.byte;
     check-ocamlopt.byte-output;
   }
 *)

(* This is a fairly subtle test that exercises an edge case of the way .cmx
   files are constructed at the time of writing the test. Essentially what is
   happening here is that in [missing_code_step1.ml] we build a functor [F],
   whose code ID has result types mentioning a second code ID for functor [G],
   whose code ID has result types mention a third code ID for functor [H].
   We export in [missing_code_step1.cmx] metadata for all 3 code IDs.

   In [missing_code_step1.ml] we simply call [F] without inlining it, which
   means we get a module mentioning the code ID of [G], which is included in
   the metadata in [missing_code_step2.cmx]. But the result types in the
   associated metadata also mention functor [H], for which we do not have
   metadata in [missing_code_step2.cmx].

   When we then call [G] in this file, we try to load its code from
   [missing_code_step1.cmx], but we have deleted that file, so we have to make
   do with its metadata, which we have from [missing_code_step2.cmx]. This
   metadata tells us that we return a function with the code ID of [H], for
   which there is no metadata in [missing_code_step2.cmx].

   At this point, we used to crash because we could not find code nor metadata
   for [H]; we now gracefully recover and simply preserve the call as is
   (behaving in the same way as if we had no information about [M.H] at all).
 *)

let x =
  let module M = Missing_code_step2.I.G(struct let y = 0 end) in
  let module N = M.H(struct let z = 0 end) in
  N.w

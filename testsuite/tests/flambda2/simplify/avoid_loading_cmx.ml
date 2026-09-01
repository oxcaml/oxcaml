(* TEST
   compile_only = "true";
   flambda2;
   readonly_files = "avoid_loading_cmx_step1.ml avoid_loading_cmx_step2.ml";
   setup-ocamlopt.byte-build-env;
   { all_modules = "avoid_loading_cmx_step1.ml";
     ocamlopt_flags += " -flambda2-result-types-all-functions";
     ocamlopt.byte;
   }{
     all_modules = "avoid_loading_cmx_step2.ml";
     ocamlopt.byte;
   }{
     script = "rm avoid_loading_cmx_step1.cmx";
     script;
   }{
     all_modules = "avoid_loading_cmx.ml";
     ocamlopt.byte;
     check-ocamlopt.byte-output;
   }
 *)

(* This test checks that simplifying an application of a function whose code
   cannot be inlined does not force loading the .cmx file that defines the
   corresponding code, as long as its metadata is available (here, re-exported
   by another .cmx file).

   In [avoid_loading_cmx_step1.ml], the function [mk] returns a closure whose
   code ID lives in that unit and cannot be inlined. Since
   [avoid_loading_cmx_step1.ml] is compiled with
   [-flambda2-result-types-all-functions], the result type of [mk] describes
   that closure, mentioning its code ID.

   In [avoid_loading_cmx_step2.ml], we call [mk] without inlining it, so the
   type of [f] is a closure type whose code ID is from
   [avoid_loading_cmx_step1.ml], and the metadata for that code ID is
   re-exported in [avoid_loading_cmx_step2.cmx].

   We then delete [avoid_loading_cmx_step1.cmx]. When we apply [f] below, its
   code ID has all the metadata needed to simplify the application (in
   particular to turn it into a direct call), and the code cannot be inlined,
   so there is no reason to look at [avoid_loading_cmx_step1.cmx] at all. We
   check that no "missing cmx" warning 58 is emitted. *)

let r = Avoid_loading_cmx_step2.f 2

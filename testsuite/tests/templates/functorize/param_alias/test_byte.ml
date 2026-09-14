(* TEST
 (* Module aliases in a *parameter interface* ([module B = A] in the
    parameter's own mli, e.g. via [include] of a shared signature).
    [-functorize] copies the parameter signature verbatim into [Make]'s
    parameter type, so the alias survives there — unlike ordinary
    functor parameters, which are scraped by
    [Mtype.scrape_for_functor_arg].

    Case 1 — concrete argument with a genuine alias: accepted, and
    coherent at runtime (the library's [B.x] reads through [A]'s field).

    Case 2 — BUG: forwarding an abstract [(P : S)] with [S] equal to the
    parameter's interface is rejected, because scraping already erased
    the alias from [P]'s type.  No abstract argument can satisfy
    [Make]'s parameter type; forwarding should be allowed.

    Case 3 — concrete argument with [B <> A]: rejected, where an
    ordinary functor with the same parameter would accept it. *)

 readonly_files = "\
   p_alias.mli uses_alias.mli uses_alias.ml \
   concrete_ok.ml test_param_alias_concrete.reference \
   forward_alias.ml forward_alias.reference \
   weird_arg.ml weird_arg.reference \
 ";

 setup-ocamlc.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p_alias uses_alias bundle";
 script;

 src = "p_alias.mli";
 dst = "p_alias/";
 copy;

 src = "uses_alias.mli uses_alias.ml";
 dst = "uses_alias/";
 copy;

 set flg_base = "-w -53";
 set flg = "$flg_base -no-alias-deps -nocwd";

 (* dune does not pass [-nocwd] to link *)
 set flg_link = "$flg_base -no-alias-deps";

 (* Parameter [P_alias], whose interface contains [module B = A]. *)

 flags = "$flg -as-parameter";
 module = "p_alias/p_alias.mli";
 ocamlc.byte;

 (* Parameterised library reading [P_alias.B.x] through the alias. *)

 flags = "$flg -parameter P_alias -I p_alias -I uses_alias";
 module = "uses_alias/uses_alias.mli uses_alias/uses_alias.ml";
 ocamlc.byte;

 (* The functorized bundle. *)

 flags = "$flg -functorize -I p_alias -I uses_alias Uses_alias";
 module = "";
 program = "bundle/bundle_alias.cmo";
 all_modules = "";
 ocamlc.byte;

 {
   (* Case 1 — concrete argument with [module B = A]: accepted, runs. *)

   flags = "$flg -I bundle -I p_alias -I uses_alias";
   module = "concrete_ok.ml";
   ocamlc.byte;

   flags = "$flg_link";
   module = "";
   program = "$test_build_directory/test_param_alias_concrete.bc";
   all_modules = "\
     uses_alias/uses_alias.cmo \
     bundle/bundle_alias.cmo \
     concrete_ok.cmo \
   ";
   ocamlc.byte;

   stdout = "test_param_alias_concrete.output";
   stderr = "test_param_alias_concrete.output";
   output = "test_param_alias_concrete.output";
   run;

   reference = "test_param_alias_concrete.reference";
   check-program-output;
 }{
   (* Case 2 — BUG: forwarding an abstract [(P : S)] is rejected. *)

   flags = "$flg -I bundle -I p_alias -I uses_alias";
   module = "forward_alias.ml";
   ocamlc_byte_exit_status = "2";
   compiler_output = "forward_alias.output";
   ocamlc.byte;

   compiler_reference = "forward_alias.reference";
   check-ocamlc.byte-output;
 }{
   (* Case 3 — concrete argument with [B <> A] is rejected. *)

   flags = "$flg -I bundle -I p_alias -I uses_alias";
   module = "weird_arg.ml";
   ocamlc_byte_exit_status = "2";
   compiler_output = "weird_arg.output";
   ocamlc.byte;

   compiler_reference = "weird_arg.reference";
   check-ocamlc.byte-output;
 }
*)

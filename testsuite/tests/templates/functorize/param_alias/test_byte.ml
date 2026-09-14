(* TEST
 (* Module aliases in a *parameter interface* ([module B = A] in the
    parameter's own mli, e.g. via [include] of a shared signature).
    Parameters are used as functor arguments, so [-as-parameter] scrapes
    such aliases the same way [Mtype.scrape_for_functor_arg] scrapes
    ordinary functor parameters: [B] becomes a concrete declaration with
    its own runtime field.

    Case 1 — concrete argument with a genuine alias: accepted, runs.

    Case 2 — forwarding an abstract [(P : S)] with [S] equal to the
    parameter's interface: accepted, since [(P : S)] scrapes the alias
    the same way.  (This used to be rejected when [-functorize] copied
    the unscraped alias into [Make]'s parameter type.)

    Case 3 — concrete argument with [B <> A]: accepted, and the
    library's [P_alias.B.x] reads [B]'s own field. *)

 readonly_files = "\
   p_alias.mli uses_alias.mli uses_alias.ml \
   concrete_ok.ml test_param_alias_concrete.reference \
   forward_alias.ml test_param_alias_forward.reference \
   weird_arg.ml test_param_alias_weird.reference \
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

 (* Parameterised library reading [P_alias.B.x]. *)

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
   (* Case 1 — concrete argument with [module B = A]. *)

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
   (* Case 2 — forwarding an abstract [(P : S)]. *)

   flags = "$flg -I bundle -I p_alias -I uses_alias";
   module = "forward_alias.ml";
   ocamlc.byte;

   flags = "$flg_link";
   module = "";
   program = "$test_build_directory/test_param_alias_forward.bc";
   all_modules = "\
     uses_alias/uses_alias.cmo \
     bundle/bundle_alias.cmo \
     forward_alias.cmo \
   ";
   ocamlc.byte;

   stdout = "test_param_alias_forward.output";
   stderr = "test_param_alias_forward.output";
   output = "test_param_alias_forward.output";
   run;

   reference = "test_param_alias_forward.reference";
   check-program-output;
 }{
   (* Case 3 — concrete argument with [B <> A] reads [B]'s own field. *)

   flags = "$flg -I bundle -I p_alias -I uses_alias";
   module = "weird_arg.ml";
   ocamlc.byte;

   flags = "$flg_link";
   module = "";
   program = "$test_build_directory/test_param_alias_weird.bc";
   all_modules = "\
     uses_alias/uses_alias.cmo \
     bundle/bundle_alias.cmo \
     weird_arg.cmo \
   ";
   ocamlc.byte;

   stdout = "test_param_alias_weird.output";
   stderr = "test_param_alias_weird.output";
   output = "test_param_alias_weird.output";
   run;

   reference = "test_param_alias_weird.reference";
   check-program-output;
 }
*)

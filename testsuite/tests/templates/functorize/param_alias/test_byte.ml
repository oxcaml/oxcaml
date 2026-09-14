(* TEST
 (* Module aliases in a parameter interface ([module B = A] in the
    parameter's mli).  [-as-parameter] scrapes them the way
    [Mtype.scrape_for_functor_arg] scrapes ordinary functor parameters,
    so [B] gets its own runtime field and the functorized bundle behaves
    like an ordinary functor: an abstract [(P : S)] can be forwarded
    into it (this used to be rejected when [-functorize] copied the
    unscraped alias into [Make]'s parameter type), and an argument with
    [B <> A] is accepted, with [B.x] reading [B]'s own field. *)

 readonly_files = "\
   p_alias.mli uses_alias.mli uses_alias.ml main.ml \
   test_param_alias.reference \
 ";

 setup-ocamlc.byte-build-env;

 set OCAMLPARAM = "";

 flags = "-as-parameter";
 module = "p_alias.mli";
 ocamlc.byte;

 flags = "-parameter P_alias";
 module = "uses_alias.mli uses_alias.ml";
 ocamlc.byte;

 flags = "-functorize Uses_alias";
 module = "";
 program = "bundle_alias.cmo";
 all_modules = "";
 ocamlc.byte;

 flags = "";
 module = "main.ml";
 ocamlc.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_param_alias.bc";
 all_modules = "uses_alias.cmo bundle_alias.cmo main.cmo";
 ocamlc.byte;

 stdout = "test_param_alias.output";
 stderr = "test_param_alias.output";
 output = "test_param_alias.output";
 run;

 reference = "test_param_alias.reference";
 check-program-output;
*)

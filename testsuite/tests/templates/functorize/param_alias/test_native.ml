(* TEST (* DO NOT EDIT. Instead edit param_alias/test_byte.ml and run gen-native.sh. *)
 (* Module aliases in a parameter interface ([module B = A] in the
    parameter's mli).  [-as-parameter] scrapes them the way
    [Mtype.scrape_for_functor_arg] scrapes ordinary functor parameters,
    so [B] gets its own runtime field and the functorized bundle behaves
    like an ordinary functor: an abstract [(P : S)] can be forwarded
    into it (this used to be rejected when [-functorize] copied the
    unscraped alias into [Make]'s parameter type), and an argument with
    [B <> A] is accepted, with [B.x] reading [B]'s own field.

    [after_alias] is a function field following the alias. Scraping only
    during [-functorize] would insert [B]'s field after [Uses_alias] had
    already compiled against the old layout: calling [after_alias] would
    try to call [B]'s module block as a closure. *)

 readonly_files = "\
   p_alias.mli uses_alias.mli uses_alias.ml main.ml \
   test_param_alias.reference \
 ";

 setup-ocamlopt.byte-build-env;

 set OCAMLPARAM = "";

 flags = "-as-parameter";
 module = "p_alias.mli";
 ocamlopt.byte;

 flags = "-parameter P_alias";
 module = "uses_alias.mli uses_alias.ml";
 ocamlopt.byte;

 flags = "-functorize Uses_alias";
 module = "";
 program = "bundle_alias.cmx";
 all_modules = "";
 ocamlopt.byte;

 flags = "";
 module = "main.ml";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_param_alias.exe";
 all_modules = "uses_alias.cmx bundle_alias.cmx main.cmx";
 ocamlopt.byte;

 stdout = "test_param_alias.output";
 stderr = "test_param_alias.output";
 output = "test_param_alias.output";
 run;

 reference = "test_param_alias.reference";
 check-program-output;
*)

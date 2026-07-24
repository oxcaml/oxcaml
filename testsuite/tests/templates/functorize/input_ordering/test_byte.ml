(* TEST
 (* [-functorize] input-list handling:

    1. Topological ordering: [-functorize Derived Basic] (wrong CLI
       order) bundles [Basic] before [Derived] via [bound_globals]
       discovery.
    2. Duplicate rejection: [-functorize Basic Basic] is caught by
       [validate_inputs]. *)

 readonly_files = "\
   derived.ml derived.mli main_functorize_derived.ml \
   test_functorize_derived.reference \
   bad_dup_input.reference \
 ";

 setup-ocamlc.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p basic derived p_int bundle_derived bundle_dup";
 script;

 src = "${test_source_directory}/../p.mli \
        ${test_source_directory}/../../dunelike/p__.ml";
 dst = "p/";
 copy;

 src = "${test_source_directory}/../../dunelike/basic.mli \
        ${test_source_directory}/../../dunelike/basic.ml \
        ${test_source_directory}/../../dunelike/basic__.ml";
 dst = "basic/";
 copy;

 src = "${test_source_directory}/../../dunelike/p_int.mli \
        ${test_source_directory}/../../dunelike/p_int.ml \
        ${test_source_directory}/../../dunelike/p_int__.ml";
 dst = "p_int/";
 copy;

 src = "derived.mli derived.ml";
 dst = "derived/";
 copy;

 set flg = "-no-alias-deps -w -53";
 set flg_int_iface = "$flg -w -49";

 (* Parameter P. *)

 flags = "$flg_int_iface";
 module = "p/p__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -I p -open P__";
 module = "p/p.mli";
 ocamlc.byte;

 (* [Basic] parameterised by P. *)

 flags = "$flg_int_iface -parameter P -I p";
 module = "basic/basic__.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -I p -I basic -open Basic__";
 module = "basic/basic.mli basic/basic.ml";
 ocamlc.byte;

 (* [P_int] argument for P. *)

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for P -I p -I p_int -open P_int__";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlc.byte;

 (* [Derived] depends on [Basic]. *)

 flags = "$flg -parameter P -I p -I basic -I derived";
 module = "derived/derived.mli derived/derived.ml";
 ocamlc.byte;

 (* Bundle [Derived Basic] (wrong order). [-functorize] re-orders so
    [Basic] precedes [Derived] in the bundle. *)

 flags = "$flg -functorize -I p -I basic -I derived Derived Basic";
 module = "";
 program = "bundle_derived/bundle_derived.cmo";
 all_modules = "";
 ocamlc.byte;

 flags = "$flg -I bundle_derived -I p -I p_int -I basic -I derived";
 module = "main_functorize_derived.ml";
 ocamlc.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize_derived.bc";
 all_modules = "\
   basic/basic__.cmo \
   basic/basic.cmo \
   derived/derived.cmo \
   p_int/p_int__.cmo \
   p_int/p_int.cmo \
   bundle_derived/bundle_derived.cmo \
   main_functorize_derived.cmo \
 ";
 ocamlc.byte;

 stdout = "test_functorize_derived.output";
 stderr = "test_functorize_derived.output";
 output = "test_functorize_derived.output";
 run;

 reference = "test_functorize_derived.reference";
 check-program-output;

 (* (2) Duplicate rejection: [-functorize Basic Basic] is a user error. *)

 flags = "$flg -functorize -I p -I basic Basic Basic";
 module = "";
 program = "bundle_dup/bundle.cmo";
 all_modules = "";
 ocamlc_byte_exit_status = "2";
 compiler_output = "bad_dup_input.output";
 ocamlc.byte;

 compiler_reference = "bad_dup_input.reference";
 check-ocamlc.byte-output;
*)

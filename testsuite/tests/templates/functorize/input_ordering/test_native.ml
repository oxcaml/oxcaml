(* TEST (* DO NOT EDIT. Instead edit input_ordering/test_byte.ml and run gen-native.sh. *)
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

 setup-ocamlopt.byte-build-env;

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
 ocamlopt.byte;

 flags = "$flg -as-parameter -I p -open P__";
 module = "p/p.mli";
 ocamlopt.byte;

 (* [Basic] parameterised by P. *)

 flags = "$flg_int_iface -parameter P -I p";
 module = "basic/basic__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -I basic -open Basic__";
 module = "basic/basic.mli basic/basic.ml";
 ocamlopt.byte;

 (* [P_int] argument for P. *)

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlopt.byte;

 flags = "$flg -as-argument-for P -I p -I p_int -open P_int__";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlopt.byte;

 (* [Derived] depends on [Basic]. *)

 flags = "$flg -parameter P -I p -I basic -I derived";
 module = "derived/derived.mli derived/derived.ml";
 ocamlopt.byte;

 (* Bundle [Derived Basic] (wrong order). [-functorize] re-orders so
    [Basic] precedes [Derived] in the bundle. *)

 flags = "$flg -functorize -I p -I basic -I derived Derived Basic";
 module = "";
 program = "bundle_derived/bundle_derived.cmx";
 all_modules = "";
 ocamlopt.byte;

 flags = "$flg -I bundle_derived -I p -I p_int -I basic -I derived";
 module = "main_functorize_derived.ml";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize_derived.exe";
 all_modules = "\
   basic/basic__.cmx \
   basic/basic.cmx \
   derived/derived.cmx \
   p_int/p_int__.cmx \
   p_int/p_int.cmx \
   bundle_derived/bundle_derived.cmx \
   main_functorize_derived.cmx \
 ";
 ocamlopt.byte;

 stdout = "test_functorize_derived.output";
 stderr = "test_functorize_derived.output";
 output = "test_functorize_derived.output";
 run;

 reference = "test_functorize_derived.reference";
 check-program-output;

 (* (2) Duplicate rejection: [-functorize Basic Basic] is a user error. *)

 flags = "$flg -functorize -I p -I basic Basic Basic";
 module = "";
 program = "bundle_dup/bundle.cmx";
 all_modules = "";
 ocamlopt_byte_exit_status = "2";
 compiler_output = "bad_dup_input.output";
 ocamlopt.byte;

 compiler_reference = "bad_dup_input.reference";
 check-ocamlopt.byte-output;
*)

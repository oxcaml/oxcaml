(* TEST
 (* Ill-formed transitive dep in an input's [bound_globals]: the input
    loads but its dep cmi is missing ([Basic.cmi] deleted after
    [Derived] was compiled against it) — surfaces
    [Persistent_env.Cmi_not_found]. *)

 readonly_files = "\
   derived.mli derived.ml \
   bad_dep_cmi_not_found.reference \
 ";

 setup-ocamlc.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p basic derived bundle";
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

 src = "derived.mli derived.ml";
 dst = "derived/";
 copy;

 set flg_base = "-w -53";
 set flg = "$flg_base -no-alias-deps -nocwd";
 set flg_int_iface = "$flg -w -49";

 (* dune does not pass [-nocwd] to link *)
 set flg_link = "$flg_base -no-alias-deps";

 (* Parameter P. *)

 flags = "$flg_int_iface";
 module = "p/p__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -H p -open-cmi p/p__.cmi";
 module = "p/p.mli";
 ocamlc.byte;

 flags = "$flg_int_iface -parameter P -I p";
 module = "basic/basic__.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -I p -H basic -open-cmi basic/basic__.cmi";
 module = "basic/basic.mli basic/basic.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -I p -I basic -I derived";
 module = "derived/derived.mli derived/derived.ml";
 ocamlc.byte;

 (* Remove [Basic.cmi] so it's no longer loadable; [Derived.cmi]'s recorded
    dep on [Basic] is now unsatisfiable. *)
 script = "rm basic/basic.cmi";
 script;

 flags = "$flg -functorize -I p -I basic -I derived Derived";
 module = "";
 program = "bundle/bundle.cmi";
 all_modules = "";
 ocamlc_byte_exit_status = "2";
 compiler_output = "bad_dep_cmi_not_found.output";
 ocamlc.byte;

 compiler_reference = "bad_dep_cmi_not_found.reference";
 check-ocamlc.byte-output;
*)

(* TEST (* DO NOT EDIT. Instead edit missing_impl/test_byte.ml and run gen-native.sh. *)
 (* Impl-mode codegen looks up each bundled unit's [.cmx]/[.cmx] on the
    load path.  With [Basic.cmx] deleted (but its [.cmi] and
    [Basic__.cmx] intact), the missing-file case must surface as a
    proper user error, not a raw [Not_found] from
    [Load_path.auto_include_libs]. *)

 readonly_files = "missing_impl_native.reference";

 setup-ocamlopt.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p basic bundle";
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

 set flg = "-no-alias-deps -w -53";
 set flg_int_iface = "$flg -w -49";

 (* Parameter P. *)

 flags = "$flg_int_iface";
 module = "p/p__.ml";
 ocamlopt.byte;

 flags = "$flg -as-parameter -I p -open P__";
 module = "p/p.mli";
 ocamlopt.byte;

 (* [Basic], parameterised by P. *)

 flags = "$flg_int_iface -parameter P -I p";
 module = "basic/basic__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -I basic -open Basic__";
 module = "basic/basic.mli basic/basic.ml";
 ocamlopt.byte;

 (* Delete [Basic]'s implementation, keeping its [.cmi] and [Basic__]'s implementation, so
    that the only implementation missing from the load path is [Basic]'s own. *)
 script = "rm basic/basic.cmx";
 script;

 flags = "$flg -functorize -I p -I basic Basic";
 module = "";
 program = "bundle/bundle.cmx";
 all_modules = "";
 ocamlopt_byte_exit_status = "2";
 compiler_output = "missing_impl.output";
 ocamlopt.byte;

 compiler_reference = "missing_impl_native.reference";
 check-ocamlopt.byte-output;
*)

(* TEST (* DO NOT EDIT. Instead edit missing_impl/test_byte.ml and run gen-native.sh. *)
 (* Impl-mode codegen looks up each bundled unit's [.cmx]/[.cmx] on the
    load path.  With [Basic.cmx] deleted (but its [.cmi] intact), the
    missing-file case must surface as a proper user error, not a raw
    [Not_found] from [Load_path.auto_include_libs]. *)

 readonly_files = "basic.mli basic.ml missing_impl_native.reference";

 setup-ocamlopt.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p basic bundle";
 script;

 src = "${test_source_directory}/../p.mli";
 dst = "p/";
 copy;

 src = "basic.mli basic.ml";
 dst = "basic/";
 copy;

 set flg = "-w -53 -no-alias-deps -nocwd";

 (* Parameter P. *)

 flags = "$flg -as-parameter";
 module = "p/p.mli";
 ocamlopt.byte;

 (* [Basic], parameterised by P. *)

 flags = "$flg -parameter P -I p -I basic";
 module = "basic/basic.mli basic/basic.ml";
 ocamlopt.byte;

 (* Delete [Basic]'s implementation, keeping its [.cmi], so the only
    implementation missing from the load path is [Basic]'s own. *)
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

(* TEST
 (* Impl-mode codegen looks up each bundled unit's [.cmo]/[.cmx] on the
    load path.  With [Basic.cmo] deleted (but its [.cmi] intact), the
    missing-file case must surface as a proper user error, not a raw
    [Not_found] from [Load_path.auto_include_libs]. *)

 readonly_files = "basic.mli basic.ml missing_impl_byte.reference";

 setup-ocamlc.byte-build-env;

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
 ocamlc.byte;

 (* [Basic], parameterised by P. *)

 flags = "$flg -parameter P -I p -I basic";
 module = "basic/basic.mli basic/basic.ml";
 ocamlc.byte;

 (* Delete [Basic]'s implementation, keeping its [.cmi], so the only
    implementation missing from the load path is [Basic]'s own. *)
 script = "rm basic/basic.cmo";
 script;

 flags = "$flg -functorize -I p -I basic Basic";
 module = "";
 program = "bundle/bundle.cmo";
 all_modules = "";
 ocamlc_byte_exit_status = "2";
 compiler_output = "missing_impl.output";
 ocamlc.byte;

 compiler_reference = "missing_impl_byte.reference";
 check-ocamlc.byte-output;
*)

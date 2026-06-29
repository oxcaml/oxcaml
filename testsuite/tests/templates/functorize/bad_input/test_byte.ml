(* TEST
 (* Invalid [-functorize] inputs — each must be a parameterised
    compunit whose cmi is on the load path.  Three ways this fails:

    1. Parameter module ([-as-parameter]): cmi's [cu] is [None].
    2. Plain module (no [-parameter]): [params] is empty.
    3. Missing cmi: [Persistent_env.Cmi_not_found] rather than a raw
       [Not_found]. *)

 readonly_files = "\
   bad_param_input.reference \
   bad_plain_input.reference \
   bad_input_cmi_not_found.reference \
 ";

 setup-ocamlc.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p plain bundle";
 script;

 src = "${test_source_directory}/../p.mli \
        ${test_source_directory}/../../dunelike/p__.ml";
 dst = "p/";
 copy;

 src = "${test_source_directory}/../plain.mli \
        ${test_source_directory}/../plain.ml";
 dst = "plain/";
 copy;

 set flg = "-no-alias-deps -nocwd -w -53";
 set flg_int_iface = "$flg -w -49";

 (* Parameter P (so it gets a [.cmi] with [cu = None]). *)

 flags = "$flg_int_iface";
 module = "p/p__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -I p -open P__";
 module = "p/p.mli";
 ocamlc.byte;

 (* Plain non-parameterised module. *)

 flags = "$flg -I plain";
 module = "plain/plain.mli plain/plain.ml";
 ocamlc.byte;

 (* Case 1: [-functorize P] where P is [-as-parameter]. *)

 flags = "$flg -functorize -I p P";
 module = "";
 program = "bundle/bundle.cmi";
 all_modules = "";
 ocamlc_byte_exit_status = "2";
 compiler_output = "bad_param_input.output";
 ocamlc.byte;

 compiler_reference = "bad_param_input.reference";
 check-ocamlc.byte-output;

 (* Case 2: [-functorize Plain] where Plain has no parameters. *)

 flags = "$flg -functorize -I plain Plain";
 module = "";
 program = "bundle/bundle.cmi";
 all_modules = "";
 ocamlc_byte_exit_status = "2";
 compiler_output = "bad_plain_input.output";
 ocamlc.byte;

 compiler_reference = "bad_plain_input.reference";
 check-ocamlc.byte-output;

 (* Case 3: [-functorize No_such_module] where the cmi can't be found. *)

 flags = "$flg -functorize No_such_module";
 module = "";
 program = "bundle/bundle.cmi";
 all_modules = "";
 ocamlc_byte_exit_status = "2";
 compiler_output = "bad_input_cmi_not_found.output";
 ocamlc.byte;

 compiler_reference = "bad_input_cmi_not_found.reference";
 check-ocamlc.byte-output;
*)

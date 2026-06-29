(* TEST (* DO NOT EDIT. Instead edit bad_input/test_byte.ml and run gen-native.sh. *)
 (* Invalid [-functorize] inputs: each input must be a parameterised
    compunit whose cmi is on the load path.  Three ways the user can
    violate that:

    1. Passing a parameter module (compiled with [-as-parameter]) — the
       cmi's [cu] is [None].

    2. Passing a non-parameterised "plain" module (no [-parameter ...] at
       compile time) — the cmi's [cu] is [Some _] but [params] is empty.

    3. Passing a module whose cmi can't be found on the load path —
       surfaces [Persistent_env.Cmi_not_found] rather than a raw
       [Not_found] backtrace. *)

 readonly_files = "\
   bad_param_input.reference \
   bad_plain_input.reference \
   bad_input_cmi_not_found.reference \
 ";

 setup-ocamlopt.byte-build-env;

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
 ocamlopt.byte;

 flags = "$flg -as-parameter -I p -open P__";
 module = "p/p.mli";
 ocamlopt.byte;

 (* Plain non-parameterised module. *)

 flags = "$flg -I plain";
 module = "plain/plain.mli plain/plain.ml";
 ocamlopt.byte;

 (* Case 1: [-functorize P] where P is [-as-parameter]. *)

 flags = "$flg -functorize -I p P";
 module = "";
 program = "bundle/bundle.cmi";
 all_modules = "";
 ocamlopt_byte_exit_status = "2";
 compiler_output = "bad_param_input.output";
 ocamlopt.byte;

 compiler_reference = "bad_param_input.reference";
 check-ocamlopt.byte-output;

 (* Case 2: [-functorize Plain] where Plain has no parameters. *)

 flags = "$flg -functorize -I plain Plain";
 module = "";
 program = "bundle/bundle.cmi";
 all_modules = "";
 ocamlopt_byte_exit_status = "2";
 compiler_output = "bad_plain_input.output";
 ocamlopt.byte;

 compiler_reference = "bad_plain_input.reference";
 check-ocamlopt.byte-output;

 (* Case 3: [-functorize No_such_module] where the cmi can't be found. *)

 flags = "$flg -functorize No_such_module";
 module = "";
 program = "bundle/bundle.cmi";
 all_modules = "";
 ocamlopt_byte_exit_status = "2";
 compiler_output = "bad_input_cmi_not_found.output";
 ocamlopt.byte;

 compiler_reference = "bad_input_cmi_not_found.reference";
 check-ocamlopt.byte-output;
*)

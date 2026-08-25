(* TEST
 (* Invalid [-functorize] inputs — each must be a parameterised
    compunit whose cmi is on the load path, given once.  Four ways this
    fails:

    1. Parameter module ([-as-parameter]): cmi's [cu] is [None].
    2. Plain module (no [-parameter]): [params] is empty.
    3. Missing cmi: [Persistent_env.Cmi_not_found] rather than a raw
       [Not_found].
    4. Duplicate input, caught by [validate_inputs] before any cmi is
       loaded. *)

 readonly_files = "\
   bad_param_input.reference \
   bad_plain_input.reference \
   bad_input_cmi_not_found.reference \
   bad_dup_input.reference \
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

 set flg_base = "-w -53";
 set flg = "$flg_base -no-alias-deps -nocwd";
 set flg_int_iface = "$flg -w -49";

 (* dune does not pass [-nocwd] to link *)
 set flg_link = "$flg_base -no-alias-deps";

 (* Parameter P (so it gets a [.cmi] with [cu = None]). *)

 flags = "$flg_int_iface";
 module = "p/p__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -H p -open-cmi p/p__.cmi";
 module = "p/p.mli";
 ocamlc.byte;

 (* Plain non-parameterised module. *)

 flags = "$flg -I plain";
 module = "plain/plain.mli plain/plain.ml";
 ocamlc.byte;

 {
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
 }{
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
 }{
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
 }{
   (* Case 4: duplicate input [-functorize Basic Basic]. *)

   flags = "$flg -functorize Basic Basic";
   module = "";
   program = "bundle/bundle.cmo";
   all_modules = "";
   ocamlc_byte_exit_status = "2";
   compiler_output = "bad_dup_input.output";
   ocamlc.byte;

   compiler_reference = "bad_dup_input.reference";
   check-ocamlc.byte-output;
 }{
   (* Case 5: a failing [-functorize] should not delete a pre-existing
      file at the [-o] path that it would never have written (the real
      outputs are [out.cmo]/[out.cmi]).

      CURRENT BEHAVIOR (bug): the error-path cleanup removes the raw
      [-o] target, deleting the unrelated pre-existing file. *)

   script = "sh -c 'echo precious > out.txt'";
   script;

   flags = "$flg -functorize No_such_module";
   module = "";
   program = "out.txt";
   all_modules = "";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;

   script = "sh -c '! test -f out.txt'";
   script;
 }
*)

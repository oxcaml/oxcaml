(* TEST
 (* Ill-formed transitive deps in an input's [bound_globals]:

    1. Cmi not found: input loads but its dep cmi is missing
       ([Basic.cmi] deleted after [Derived] was compiled against it) —
       surfaces [Persistent_env.Cmi_not_found].
    2. Cmi CRC mismatch: [basic_pq__.cmi] is recompiled with fewer
       parameters after [User_pq] was compiled against the older CRC —
       Consistbl catches it. *)

 readonly_files = "\
   derived.mli derived.ml \
   basic_pq.mli basic_pq.ml basic_pq__.ml \
   user_pq.mli user_pq.ml user_pq__.ml \
   bad_dep_cmi_not_found.reference \
   bad_dep_cmi_changed.reference \
 ";

 setup-ocamlc.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p q basic derived bundle";
 script;

 src = "${test_source_directory}/../p.mli \
        ${test_source_directory}/../../dunelike/p__.ml";
 dst = "p/";
 copy;

 src = "${test_source_directory}/../../dunelike/q.mli \
        ${test_source_directory}/../../dunelike/q__.ml";
 dst = "q/";
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

 (* Parameter Q (used by case 2). *)

 flags = "$flg_int_iface";
 module = "q/q__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -H q -open-cmi q/q__.cmi";
 module = "q/q.mli";
 ocamlc.byte;

 (* ===== Case 1: dep cmi not found ===== *)

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

 (* ===== Case 2: dep cmi changed (CRC mismatch) ===== *)

 (* [Basic_pq] is compiled with -parameter P -parameter Q first. *)

 flags = "$flg_int_iface -parameter P -parameter Q -I p -I q";
 module = "basic_pq__.ml";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;

 flags = "$flg -parameter P -parameter Q -I p -I q -open-cmi basic_pq__.cmi";
 module = "basic_pq.mli basic_pq.ml";
 ocamlc.byte;

 (* [User_pq] references [Basic_pq] from a -parameter P -parameter Q
    context — records [Basic_pq__]'s CRC v1. *)

 flags = "$flg_int_iface -parameter P -parameter Q -I p -I q";
 module = "user_pq__.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -parameter Q -I p -I q -open-cmi user_pq__.cmi";
 module = "user_pq.mli user_pq.ml";
 ocamlc.byte;

 (* Recompile [Basic_pq] with only -parameter P, overwriting [basic_pq__]'s
    cmi (v2 with a different CRC). *)

 flags = "$flg_int_iface -parameter P -I p";
 module = "basic_pq__.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -I p -open-cmi basic_pq__.cmi";
 module = "basic_pq.mli basic_pq.ml";
 ocamlc.byte;

 flags = "$flg -functorize -I p -I q User_pq";
 module = "";
 program = "bundle/bundle.cmi";
 all_modules = "";
 ocamlc_byte_exit_status = "2";
 compiler_output = "bad_dep_cmi_changed.output";
 ocamlc.byte;

 compiler_reference = "bad_dep_cmi_changed.reference";
 check-ocamlc.byte-output;
*)

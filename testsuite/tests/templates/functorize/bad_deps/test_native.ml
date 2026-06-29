(* TEST (* DO NOT EDIT. Instead edit bad_deps/test_byte.ml and run gen-native.sh. *)
 (* Bad transitive dependencies: three ways a dep referenced by an input's
    [bound_globals] can be ill-formed when [-functorize] tries to walk it.

    1. Dep cmi not found: input loads fine, but loading the dep cmi fails.
       Setup: compile [Basic] then [Derived] (which records [Basic] as an
       [Exact]-precision dep), remove [Basic.cmi], then [-functorize
       Derived].  Surfaces [Persistent_env.Cmi_not_found].

    2. Dep cmi changed (CRC mismatch): the input records dep's CRC v1, but
       the on-disk dep cmi has been replaced with v2.  Setup: compile
       [basic_pq__] with [-parameter P -parameter Q] (cmi v1), compile
       [user_pq] (records v1's CRC for [basic_pq__]), then recompile
       [basic_pq__] with just [-parameter P] (cmi v2).  [-functorize
       User_pq] triggers Consistbl detection between [user_pq.cmi]'s
       imports and the on-disk [basic_pq__.cmi].

    3. Dep is a partial instance: input's [bound_globals] contains a
       [GM.t] with non-empty [visible_args] (a partial application like
       [Pair_pq[Q:Q_impl]]).  [-functorize] only supports fully-uninstantiated
       references; [validate_and_load] rejects on [visible_args <> []]. *)

 readonly_files = "\
   derived.mli derived.ml \
   basic_pq.mli basic_pq.ml basic_pq__.ml \
   user_pq.mli user_pq.ml user_pq__.ml \
   pair_pq.mli pair_pq.ml pair_pq__.ml \
   partial_pq.mli partial_pq.ml partial_pq__.ml \
   bad_dep_cmi_not_found.reference \
   bad_dep_cmi_changed.reference \
   bad_partial_input.reference \
 ";

 setup-ocamlopt.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p q q_impl basic derived bundle";
 script;

 src = "${test_source_directory}/../p.mli \
        ${test_source_directory}/../../dunelike/p__.ml";
 dst = "p/";
 copy;

 src = "${test_source_directory}/../../dunelike/q.mli \
        ${test_source_directory}/../../dunelike/q__.ml";
 dst = "q/";
 copy;

 src = "${test_source_directory}/../../dunelike/q_impl.ml \
        ${test_source_directory}/../../dunelike/q_impl__.ml";
 dst = "q_impl/";
 copy;

 src = "${test_source_directory}/../../dunelike/basic.mli \
        ${test_source_directory}/../../dunelike/basic.ml \
        ${test_source_directory}/../../dunelike/basic__.ml";
 dst = "basic/";
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

 (* Parameter Q (used by cases 2 and 3). *)

 flags = "$flg_int_iface";
 module = "q/q__.ml";
 ocamlopt.byte;

 flags = "$flg -as-parameter -I q -open Q__";
 module = "q/q.mli";
 ocamlopt.byte;

 (* ===== Case 1: dep cmi not found ===== *)

 flags = "$flg_int_iface -parameter P -I p";
 module = "basic/basic__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -I basic -open Basic__";
 module = "basic/basic.mli basic/basic.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -I basic -I derived";
 module = "derived/derived.mli derived/derived.ml";
 ocamlopt.byte;

 (* Remove [Basic.cmi] so it's no longer loadable; [Derived.cmi]'s recorded
    dep on [Basic] is now unsatisfiable. *)
 script = "rm basic/basic.cmi";
 script;

 flags = "$flg -functorize -I p -I basic -I derived Derived";
 module = "";
 program = "bundle/bundle.cmi";
 all_modules = "";
 ocamlopt_byte_exit_status = "2";
 compiler_output = "bad_dep_cmi_not_found.output";
 ocamlopt.byte;

 compiler_reference = "bad_dep_cmi_not_found.reference";
 check-ocamlopt.byte-output;

 (* ===== Case 2: dep cmi changed (CRC mismatch) ===== *)

 (* [Basic_pq] is compiled with -parameter P -parameter Q first. *)

 flags = "$flg_int_iface -parameter P -parameter Q -I p -I q";
 module = "basic_pq__.ml";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;

 flags = "$flg -parameter P -parameter Q -I p -I q -open Basic_pq__";
 module = "basic_pq.mli basic_pq.ml";
 ocamlopt.byte;

 (* [User_pq] references [Basic_pq] from a -parameter P -parameter Q
    context — records [Basic_pq__]'s CRC v1. *)

 flags = "$flg_int_iface -parameter P -parameter Q -I p -I q";
 module = "user_pq__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -parameter Q -I p -I q -open User_pq__";
 module = "user_pq.mli user_pq.ml";
 ocamlopt.byte;

 (* Recompile [Basic_pq] with only -parameter P, overwriting [basic_pq__]'s
    cmi (v2 with a different CRC). *)

 flags = "$flg_int_iface -parameter P -I p";
 module = "basic_pq__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -open Basic_pq__";
 module = "basic_pq.mli basic_pq.ml";
 ocamlopt.byte;

 flags = "$flg -functorize -I p -I q User_pq";
 module = "";
 program = "bundle/bundle.cmi";
 all_modules = "";
 ocamlopt_byte_exit_status = "2";
 compiler_output = "bad_dep_cmi_changed.output";
 ocamlopt.byte;

 compiler_reference = "bad_dep_cmi_changed.reference";
 check-ocamlopt.byte-output;

 (* ===== Case 3: dep is a partial instance ===== *)

 (* [Q_impl] argument for Q (parameterised by P so its elaborated form's
    hidden_args contain P). *)

 flags = "$flg_int_iface";
 module = "q_impl/q_impl__.ml";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;

 flags = "$flg -as-argument-for Q -parameter P -I p -I q -I q_impl \
   -open Q_impl__";
 module = "q_impl/q_impl.ml";
 ocamlopt.byte;

 (* [Pair_pq] parameterised by both P and Q. *)

 flags = "$flg_int_iface -parameter P -parameter Q -I p -I q";
 module = "pair_pq__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -parameter Q -I p -I q -open Pair_pq__";
 module = "pair_pq.mli pair_pq.ml";
 ocamlopt.byte;

 (* [Partial_pq] parameterised by P only; references the partial instance
    [Pair_pq[Q:Q_impl]] via [include Pair_pq(Q)(Q_impl)] (forces an
    [Exact]-precision bound_global with non-empty [visible_args]). *)

 flags = "$flg_int_iface -parameter P -I p -I q -I q_impl";
 module = "partial_pq__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -I q -I q_impl -open Partial_pq__";
 module = "partial_pq.mli partial_pq.ml";
 ocamlopt.byte;

 flags = "$flg -functorize -I p -I q -I q_impl Partial_pq";
 module = "";
 program = "bundle/bundle.cmi";
 all_modules = "";
 ocamlopt_byte_exit_status = "2";
 compiler_output = "bad_partial_input.output";
 ocamlopt.byte;

 compiler_reference = "bad_partial_input.reference";
 check-ocamlopt.byte-output;
*)

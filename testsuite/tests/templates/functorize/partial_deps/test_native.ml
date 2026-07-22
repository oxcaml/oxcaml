(* TEST (* DO NOT EDIT. Instead edit partial_deps/test_byte.ml and run gen-native.sh. *)
 (* Bound_globals with parameterised arguments — see each case below. *)

 readonly_files = "\
   foo_q.mli foo_q.ml foo_q__.ml \
   nested_arg.mli nested_arg.ml nested_arg__.ml \
   pair_pq.mli pair_pq.ml pair_pq__.ml \
   partial_pq.mli partial_pq.ml partial_pq__.ml \
   main_nested_arg.ml test_functorize_nested_arg.reference \
   main_partial_pq.ml test_functorize_partial_pq.reference \
 ";

 setup-ocamlopt.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p p_int q q_impl foo_q nested_arg pair_pq \
                 partial_pq bundle_nested bundle_partial";
 script;

 src = "${test_source_directory}/../p.mli \
        ${test_source_directory}/../../dunelike/p__.ml";
 dst = "p/";
 copy;

 src = "${test_source_directory}/../../dunelike/p_int.mli \
        ${test_source_directory}/../../dunelike/p_int.ml \
        ${test_source_directory}/../../dunelike/p_int__.ml";
 dst = "p_int/";
 copy;

 src = "${test_source_directory}/../../dunelike/q.mli \
        ${test_source_directory}/../../dunelike/q__.ml";
 dst = "q/";
 copy;

 src = "${test_source_directory}/../../dunelike/q_impl.ml \
        ${test_source_directory}/../../dunelike/q_impl__.ml";
 dst = "q_impl/";
 copy;

 src = "foo_q.mli foo_q.ml foo_q__.ml";
 dst = "foo_q/";
 copy;

 src = "nested_arg.mli nested_arg.ml nested_arg__.ml";
 dst = "nested_arg/";
 copy;

 src = "pair_pq.mli pair_pq.ml pair_pq__.ml";
 dst = "pair_pq/";
 copy;

 src = "partial_pq.mli partial_pq.ml partial_pq__.ml";
 dst = "partial_pq/";
 copy;

 set flg = "-no-alias-deps -w -53";
 set flg_int_iface = "$flg -w -49";

 (* Parameter P and argument P_int. *)

 flags = "$flg_int_iface";
 module = "p/p__.ml";
 ocamlopt.byte;

 flags = "$flg -as-parameter -I p -open P__";
 module = "p/p.mli";
 ocamlopt.byte;

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlopt.byte;

 flags = "$flg -as-argument-for P -I p -I p_int -open P_int__";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlopt.byte;

 (* Parameter Q and argument Q_int. *)

 flags = "$flg_int_iface";
 module = "q/q__.ml";
 ocamlopt.byte;

 flags = "$flg -as-parameter -I q -open Q__";
 module = "q/q.mli";
 ocamlopt.byte;

 (* Q_impl: -parameter P -as-argument-for Q — a parameterised argument. *)

 flags = "$flg_int_iface";
 module = "q_impl/q_impl__.ml";
 ocamlopt.byte;

 flags = "$flg -as-argument-for Q -parameter P -I p -I q -I q_impl \
   -open Q_impl__";
 module = "q_impl/q_impl.ml";
 ocamlopt.byte;

 (* ===== Case 1: Nested_arg — Foo_q[Q:Q_impl{P}] ===== *)

 (* Step 1: build [Foo_q], a plain library parameterised by [Q]. *)

 flags = "$flg_int_iface -parameter Q -I q";
 module = "foo_q/foo_q__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter Q -I p -I q -I foo_q -open Foo_q__";
 module = "foo_q/foo_q.mli foo_q/foo_q.ml";
 ocamlopt.byte;

 (* Step 2: build [Nested_arg] (parameterised by [P]) whose body
    references [Foo_q[Q:Q_impl{P}]] — [Foo_q] applied to the
    parameterised argument [Q_impl].  This lands the compound
    reference in [Nested_arg]'s bound_globals. *)

 flags = "$flg_int_iface -parameter P -I p -I q -I q_impl -I foo_q";
 module = "nested_arg/nested_arg__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -I q -I q_impl -I foo_q \
   -I nested_arg -open Nested_arg__";
 module = "nested_arg/nested_arg.mli nested_arg/nested_arg.ml";
 ocamlopt.byte;

 (* Step 3: functorize [Nested_arg].  [Foo_q] and [Q_impl] are pulled
    in transitively and the compound reference is specialised in the
    result. *)

 flags = "$flg -functorize -I p -I q -I q_impl -I foo_q -I nested_arg \
   Nested_arg";
 module = "";
 program = "bundle_nested/bundle.cmx";
 all_modules = "";
 ocamlopt.byte;

 (* Step 4: consume the result by applying [Bundle.Make (P_int) ()]
    and printing [Inst.Nested_arg.describe] (which composes strings
    from [Nested_arg] and, via the compound alias, [Foo_q]). *)

 flags = "$flg -I bundle_nested -I p -I p_int -I q -I q_impl \
   -I foo_q -I nested_arg";
 module = "main_nested_arg.ml";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize_nested_arg.exe";
 all_modules = "\
   foo_q/foo_q__.cmx \
   foo_q/foo_q.cmx \
   q_impl/q_impl__.cmx \
   q_impl/q_impl.cmx \
   nested_arg/nested_arg__.cmx \
   nested_arg/nested_arg.cmx \
   p_int/p_int__.cmx \
   p_int/p_int.cmx \
   bundle_nested/bundle.cmx \
   main_nested_arg.cmx \
 ";
 ocamlopt.byte;

 stdout = "test_functorize_nested_arg.output";
 stderr = "test_functorize_nested_arg.output";
 output = "test_functorize_nested_arg.output";
 run;

 reference = "test_functorize_nested_arg.reference";
 check-program-output;

 (* ===== Case 2: Partial_pq — Pair_pq[Q:Q_impl{P}]{P} ===== *)

 (* Step 1: build [Pair_pq], a plain library parameterised by both
    [P] and [Q]. *)

 flags = "$flg_int_iface -parameter P -parameter Q -I p -I q";
 module = "pair_pq/pair_pq__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -parameter Q -I p -I q -I pair_pq \
   -open Pair_pq__";
 module = "pair_pq/pair_pq.mli pair_pq/pair_pq.ml";
 ocamlopt.byte;

 (* Step 2: build [Partial_pq] (parameterised by [P] only) whose body
    references [Pair_pq[Q:Q_impl{P}]{P}] — [Pair_pq]'s [Q] is filled
    by [Q_impl], but its [P] is left unfilled at the top. *)

 flags = "$flg_int_iface -parameter P -I p -I q -I q_impl -I pair_pq";
 module = "partial_pq/partial_pq__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -I q -I q_impl -I pair_pq -I partial_pq \
   -open Partial_pq__";
 module = "partial_pq/partial_pq.mli partial_pq/partial_pq.ml";
 ocamlopt.byte;

 (* Step 3: functorize [Partial_pq].  [Pair_pq] and [Q_impl] are
    pulled in and the compound reference is specialised. *)

 flags = "$flg -functorize -I p -I q -I q_impl -I pair_pq -I partial_pq \
   Partial_pq";
 module = "";
 program = "bundle_partial/bundle.cmx";
 all_modules = "";
 ocamlopt.byte;

 (* Step 4: consume the result by applying [Bundle.Make (P_int) ()]
    and printing [Inst.Partial_pq.describe]. *)

 flags = "$flg -I bundle_partial -I p -I p_int -I q -I q_impl -I pair_pq \
   -I partial_pq";
 module = "main_partial_pq.ml";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize_partial_pq.exe";
 all_modules = "\
   pair_pq/pair_pq__.cmx \
   pair_pq/pair_pq.cmx \
   q_impl/q_impl__.cmx \
   q_impl/q_impl.cmx \
   partial_pq/partial_pq__.cmx \
   partial_pq/partial_pq.cmx \
   p_int/p_int__.cmx \
   p_int/p_int.cmx \
   bundle_partial/bundle.cmx \
   main_partial_pq.cmx \
 ";
 ocamlopt.byte;

 stdout = "test_functorize_partial_pq.output";
 stderr = "test_functorize_partial_pq.output";
 output = "test_functorize_partial_pq.output";
 run;

 reference = "test_functorize_partial_pq.reference";
 check-program-output;
*)

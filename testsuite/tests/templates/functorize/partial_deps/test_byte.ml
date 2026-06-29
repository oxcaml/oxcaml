(* TEST
 (* Bound_globals with parameterised arguments — two shapes:

    1. [Nested_arg]'s [Foo_q(Q)(Q_impl)]: [visible_args] non-empty,
       [hidden_args] empty at the top level but nested inside [Q_impl]
       (has hidden [P]).

    2. [Partial_pq]'s [Pair_pq[Q:Q_impl]]: [visible_args] and
       [hidden_args] both non-empty ([P] still unfilled at the top).

    Both bundle cleanly — the head and its parameterised argument are
    pulled in transitively and the compound reference is specialised
    in the bundle. *)

 readonly_files = "\
   foo_q.mli foo_q.ml foo_q__.ml \
   nested_arg.mli nested_arg.ml nested_arg__.ml \
   pair_pq.mli pair_pq.ml pair_pq__.ml \
   partial_pq.mli partial_pq.ml partial_pq__.ml \
   q_int.mli q_int.ml \
   main_nested_arg.ml test_functorize_nested_arg.reference \
   main_partial_pq.ml test_functorize_partial_pq.reference \
 ";

 setup-ocamlc.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p p_int q q_int q_impl foo_q nested_arg pair_pq \
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

 src = "q_int.mli q_int.ml";
 dst = "q_int/";
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
 ocamlc.byte;

 flags = "$flg -as-parameter -I p -open P__";
 module = "p/p.mli";
 ocamlc.byte;

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for P -I p -I p_int -open P_int__";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlc.byte;

 (* Parameter Q and argument Q_int. *)

 flags = "$flg_int_iface";
 module = "q/q__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -I q -open Q__";
 module = "q/q.mli";
 ocamlc.byte;

 flags = "$flg -as-argument-for Q -I q -I q_int";
 module = "q_int/q_int.mli q_int/q_int.ml";
 ocamlc.byte;

 (* Q_impl: -parameter P -as-argument-for Q — a parameterised argument. *)

 flags = "$flg_int_iface";
 module = "q_impl/q_impl__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for Q -parameter P -I p -I q -I q_impl \
   -open Q_impl__";
 module = "q_impl/q_impl.ml";
 ocamlc.byte;

 (* ===== Case 1: Nested_arg — Foo_q[Q:Q_impl{P}] (hidden P nested in arg) ===== *)

 (* Foo_q: parameterised by Q only. *)

 flags = "$flg_int_iface -parameter Q -I q";
 module = "foo_q/foo_q__.ml";
 ocamlc.byte;

 flags = "$flg -parameter Q -I p -I q -I foo_q -open Foo_q__";
 module = "foo_q/foo_q.mli foo_q/foo_q.ml";
 ocamlc.byte;

 (* Nested_arg: parameterised by P and Q; uses [Foo_q(Q)(Q_impl)]. *)

 flags = "$flg_int_iface -parameter P -parameter Q -I p -I q -I q_impl \
   -I foo_q";
 module = "nested_arg/nested_arg__.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -parameter Q -I p -I q -I q_impl -I foo_q \
   -I nested_arg -open Nested_arg__";
 module = "nested_arg/nested_arg.mli nested_arg/nested_arg.ml";
 ocamlc.byte;

 flags = "$flg -functorize -I p -I q -I q_impl -I foo_q -I nested_arg \
   Nested_arg";
 module = "";
 program = "bundle_nested/bundle.cmo";
 all_modules = "";
 ocamlc.byte;

 flags = "$flg -I bundle_nested -I p -I p_int -I q -I q_int -I q_impl \
   -I foo_q -I nested_arg";
 module = "main_nested_arg.ml";
 ocamlc.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize_nested_arg.bc";
 all_modules = "\
   foo_q/foo_q__.cmo \
   foo_q/foo_q.cmo \
   q_impl/q_impl__.cmo \
   q_impl/q_impl.cmo \
   nested_arg/nested_arg__.cmo \
   nested_arg/nested_arg.cmo \
   p_int/p_int__.cmo \
   p_int/p_int.cmo \
   q_int/q_int.cmo \
   bundle_nested/bundle.cmo \
   main_nested_arg.cmo \
 ";
 ocamlc.byte;

 stdout = "test_functorize_nested_arg.output";
 stderr = "test_functorize_nested_arg.output";
 output = "test_functorize_nested_arg.output";
 run;

 reference = "test_functorize_nested_arg.reference";
 check-program-output;

 (* ===== Case 2: Partial_pq — Pair_pq[Q:Q_impl]{P} (partial instance) ===== *)

 (* Pair_pq: parameterised by both P and Q. *)

 flags = "$flg_int_iface -parameter P -parameter Q -I p -I q";
 module = "pair_pq/pair_pq__.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -parameter Q -I p -I q -I pair_pq \
   -open Pair_pq__";
 module = "pair_pq/pair_pq.mli pair_pq/pair_pq.ml";
 ocamlc.byte;

 (* Partial_pq: parameterised by P only; references [Pair_pq(Q)(Q_impl)],
    yielding a partial instance in its bound_globals. *)

 flags = "$flg_int_iface -parameter P -I p -I q -I q_impl -I pair_pq";
 module = "partial_pq/partial_pq__.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -I p -I q -I q_impl -I pair_pq -I partial_pq \
   -open Partial_pq__";
 module = "partial_pq/partial_pq.mli partial_pq/partial_pq.ml";
 ocamlc.byte;

 flags = "$flg -functorize -I p -I q -I q_impl -I pair_pq -I partial_pq \
   Partial_pq";
 module = "";
 program = "bundle_partial/bundle.cmo";
 all_modules = "";
 ocamlc.byte;

 flags = "$flg -I bundle_partial -I p -I p_int -I q -I q_impl -I pair_pq \
   -I partial_pq";
 module = "main_partial_pq.ml";
 ocamlc.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize_partial_pq.bc";
 all_modules = "\
   pair_pq/pair_pq__.cmo \
   pair_pq/pair_pq.cmo \
   q_impl/q_impl__.cmo \
   q_impl/q_impl.cmo \
   partial_pq/partial_pq__.cmo \
   partial_pq/partial_pq.cmo \
   p_int/p_int__.cmo \
   p_int/p_int.cmo \
   bundle_partial/bundle.cmo \
   main_partial_pq.cmo \
 ";
 ocamlc.byte;

 stdout = "test_functorize_partial_pq.output";
 stderr = "test_functorize_partial_pq.output";
 output = "test_functorize_partial_pq.output";
 run;

 reference = "test_functorize_partial_pq.reference";
 check-program-output;
*)

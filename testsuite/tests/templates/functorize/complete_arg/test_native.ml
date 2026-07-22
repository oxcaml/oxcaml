(* TEST (* DO NOT EDIT. Instead edit complete_arg/test_byte.ml and run gen-native.sh. *)
 (* A compound reference [Pair_pq[P:P_stateful[A:A_impl]]{Q}] where the
    arg value [P_stateful[A:A_impl]] is a complete (fully-instantiated)
    parameterised module.  Verifies that the functorizer resolves
    [P_stateful[A:A_impl]] to a [Pgetglobal] of that specific
    compilation unit — so the counter is shared between direct access
    (via [Static]) and access via the bundle. *)

 readonly_files = "\
   p.mli a.mli \
   a_impl.mli a_impl.ml \
   p_stateful.mli p_stateful.ml \
   pair_pq.mli pair_pq.ml \
   bar_q.mli bar_q.ml \
   q_int.mli q_int.ml \
   main_stateful.ml test_stateful.reference \
 ";

 setup-ocamlopt.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p q a a_impl p_stateful q_int pair_pq bar_q \
                 instances bundle_bar";
 script;

 src = "p.mli";     dst = "p/";        copy;
 src = "a.mli";     dst = "a/";        copy;
 src = "${test_source_directory}/../../dunelike/q.mli \
        ${test_source_directory}/../../dunelike/q__.ml";
 dst = "q/";
 copy;
 src = "a_impl.mli a_impl.ml";       dst = "a_impl/";     copy;
 src = "p_stateful.mli p_stateful.ml"; dst = "p_stateful/"; copy;
 src = "q_int.mli q_int.ml";         dst = "q_int/";      copy;
 src = "pair_pq.mli pair_pq.ml";     dst = "pair_pq/";    copy;
 src = "bar_q.mli bar_q.ml";         dst = "bar_q/";  copy;

 set flg = "-no-alias-deps -w -53";
 set flg_int_iface = "$flg -w -49";

 (* Step 1: build parameters [P], [Q], and [A]. *)

 flags = "$flg -as-parameter -I p";
 module = "p/p.mli";
 ocamlopt.byte;

 flags = "$flg_int_iface";
 module = "q/q__.ml";
 ocamlopt.byte;

 flags = "$flg -as-parameter -I q -open Q__";
 module = "q/q.mli";
 ocamlopt.byte;

 flags = "$flg -as-parameter -I a";
 module = "a/a.mli";
 ocamlopt.byte;

 (* Step 2: build [A_impl] (argument for [A]) and [Q_int] (trivial
    argument for [Q]). *)

 flags = "$flg -as-argument-for A -I a -I a_impl";
 module = "a_impl/a_impl.mli a_impl/a_impl.ml";
 ocamlopt.byte;

 flags = "$flg -as-argument-for Q -I q -I q_int";
 module = "q_int/q_int.mli q_int/q_int.ml";
 ocamlopt.byte;

 (* Step 3: build [P_stateful], parameterised by [A] and used as an
    argument for [P].  Has a module-level counter — a *fresh* counter
    for each instantiation of [P_stateful]. *)

 flags = "$flg -as-argument-for P -parameter A -I p -I a -I p_stateful";
 module = "p_stateful/p_stateful.mli p_stateful/p_stateful.ml";
 ocamlopt.byte;

 (* Step 4: pre-instantiate [P_stateful[A:A_impl]] into [instances/] —
    this creates the static compilation unit whose counter will be
    shared. *)

 flags = "$flg -I p -I a -I a_impl -I p_stateful -instantiate";
 module = "";
 program = "instances/p_stateful-A_impl.cmx";
 all_modules = "p_stateful/p_stateful.cmx a_impl/a_impl.cmx";
 ocamlopt.byte;

 (* Step 5: build [Pair_pq], parameterised by both [P] and [Q].  Its
    [bump] calls [P.inc_count ()]. *)

 flags = "$flg -parameter P -parameter Q -I p -I q -I pair_pq";
 module = "pair_pq/pair_pq.mli pair_pq/pair_pq.ml";
 ocamlopt.byte;

 (* Step 6: build [Bar_q] (parameterised by [Q] only) whose body
    references [Pair_pq[P:P_stateful[A:A_impl]]{Q}] — the visible arg
    for [P] is the complete instance [P_stateful[A:A_impl]]. *)

 flags = "$flg -parameter Q -I p -I a -I a_impl -I p_stateful \
   -I instances -I q -I pair_pq -I bar_q";
 module = "bar_q/bar_q.mli bar_q/bar_q.ml";
 ocamlopt.byte;

 (* Step 7: functorize [Bar_q].  The complete arg [P_stateful[A:A_impl]]
    is resolved via [Pgetglobal] to the pre-instantiated compilation
    unit — no local re-instantiation. *)

 flags = "$flg -functorize -I p -I a -I a_impl -I p_stateful \
   -I instances -I q -I pair_pq -I bar_q Bar_q";
 module = "";
 program = "bundle_bar/bundle.cmx";
 all_modules = "";
 ocamlopt.byte;

 (* Step 8: consume the bundle.  [main_stateful.ml] aliases the static
    instance via [@jane.non_erasable.instances] and observes that its
    counter is shared with the bundle's usage. *)

 flags = "$flg -I bundle_bar -I p -I a -I a_impl -I p_stateful \
   -I instances -I q -I q_int -I pair_pq -I bar_q";
 module = "main_stateful.ml";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_stateful.exe";
 all_modules = "\
   a_impl/a_impl.cmx \
   p_stateful/p_stateful.cmx \
   instances/p_stateful-A_impl.cmx \
   q_int/q_int.cmx \
   pair_pq/pair_pq.cmx \
   bar_q/bar_q.cmx \
   bundle_bar/bundle.cmx \
   main_stateful.cmx \
 ";
 ocamlopt.byte;

 stdout = "test_stateful.output";
 stderr = "test_stateful.output";
 output = "test_stateful.output";
 run;

 reference = "test_stateful.reference";
 check-program-output;
*)

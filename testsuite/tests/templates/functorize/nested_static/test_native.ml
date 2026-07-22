(* TEST (* DO NOT EDIT. Instead edit nested_static/test_byte.ml and run gen-native.sh. *)
 (* [Bar] is parameterised by [P, Q] and its body uses [Stateful] (which
    is only parameterised by [P]), so [Stateful] appears as a runtime
    parameter of [Bar].  [Wrap] references [Bar[P:P_int]{Q}].

    Functorizing [Wrap] exercises the [Rp_main_module_block] path in
    [bind_local_instance]: the runtime slot [Stateful[P:P]] is
    substituted with [visible_arg_map = {P:P_int}] to become
    [Stateful[P:P_int]] — now complete.  The functorizer resolves it
    to a [Pgetglobal] of the pre-instantiated [Stateful[P:P_int]]
    compilation unit, so its counter is a shared global instance. *)

 readonly_files = "\
   stateful.mli stateful.ml \
   bar.mli bar.ml \
   wrap.mli wrap.ml \
   q_int.mli q_int.ml \
   main_nested.ml test_nested.reference \
 ";

 setup-ocamlopt.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p p_int q q_int stateful bar wrap instances bundle_wrap";
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

 src = "q_int.mli q_int.ml";         dst = "q_int/";     copy;
 src = "stateful.mli stateful.ml";   dst = "stateful/";  copy;
 src = "bar.mli bar.ml";             dst = "bar/";       copy;
 src = "wrap.mli wrap.ml";           dst = "wrap/";      copy;

 set flg = "-no-alias-deps -w -53";
 set flg_int_iface = "$flg -w -49";

 (* Step 1: parameters [P] and [Q], and argument [P_int]. *)

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

 flags = "$flg_int_iface";
 module = "q/q__.ml";
 ocamlopt.byte;

 flags = "$flg -as-parameter -I q -open Q__";
 module = "q/q.mli";
 ocamlopt.byte;

 flags = "$flg -as-argument-for Q -I q -I q_int";
 module = "q_int/q_int.mli q_int/q_int.ml";
 ocamlopt.byte;

 (* Step 2: build [Stateful], parameterised by [P] — has a module-level
    counter. *)

 flags = "$flg -parameter P -I p -I stateful";
 module = "stateful/stateful.mli stateful/stateful.ml";
 ocamlopt.byte;

 (* Step 3: pre-instantiate [Stateful[P:P_int]] into [instances/]. *)

 flags = "$flg -I p -I p_int -I stateful -instantiate";
 module = "";
 program = "instances/stateful-P_int.cmx";
 all_modules = "stateful/stateful.cmx p_int/p_int.cmx";
 ocamlopt.byte;

 (* Step 4: build [Bar], parameterised by [P] and [Q].  Its body
    references [Stateful], so [Stateful] becomes a runtime parameter of
    [Bar]. *)

 flags = "$flg -parameter P -parameter Q -I p -I q -I stateful -I bar";
 module = "bar/bar.mli bar/bar.ml";
 ocamlopt.byte;

 (* Step 5: build [Wrap] (parameterised by [Q] only) whose body
    references [Bar(P)(P_int)] — i.e. the compound
    [Bar[P:P_int]{Q}]. *)

 flags = "$flg -parameter Q -I p -I p_int -I q -I stateful -I bar -I wrap";
 module = "wrap/wrap.mli wrap/wrap.ml";
 ocamlopt.byte;

 (* Step 6: functorize [Wrap].  Bar's [Rp_main_module_block Stateful[P:P]]
    is substituted to [Stateful[P:P_int]] (complete) and resolved to a
    [Pgetglobal] of the pre-instantiated CU. *)

 flags = "$flg -functorize -I p -I p_int -I q -I stateful -I instances \
   -I bar -I wrap Wrap";
 module = "";
 program = "bundle_wrap/bundle.cmx";
 all_modules = "";
 ocamlopt.byte;

 (* Step 7: main verifies that [Stateful[P:P_int]]'s counter is shared
    between direct access via [Static] and access via the bundle. *)

 flags = "$flg -I bundle_wrap -I p -I p_int -I q -I q_int -I stateful \
   -I instances -I bar -I wrap";
 module = "main_nested.ml";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_nested.exe";
 all_modules = "\
   p_int/p_int__.cmx \
   p_int/p_int.cmx \
   stateful/stateful.cmx \
   instances/stateful-P_int.cmx \
   q_int/q_int.cmx \
   bar/bar.cmx \
   wrap/wrap.cmx \
   bundle_wrap/bundle.cmx \
   main_nested.cmx \
 ";
 ocamlopt.byte;

 stdout = "test_nested.output";
 stderr = "test_nested.output";
 output = "test_nested.output";
 run;

 reference = "test_nested.reference";
 check-program-output;
*)

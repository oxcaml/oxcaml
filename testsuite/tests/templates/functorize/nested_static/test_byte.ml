(* TEST
 (* Nested compound reference whose runtime dependency becomes complete
    ("static") after substitution.

    (a) Quasi-OCaml, writing parameterised units as functors:

    {[
      module Stateful (P : P) = struct
        let counter = ref 0
        let inc_count () = incr counter
        let get_count () = !counter
      end
      module Bar (P : P) (Q : Q) = struct
        module Stateful = Stateful (P)         (* runtime dependency *)
        let foo_count () = Stateful.get_count ()
        let foo_bump () = Stateful.inc_count ()
      end
      module Wrap (Q : Q) = struct
        module Bar_of_p_int = Bar (P_int) (Q)
        let foo_count () = Bar_of_p_int.foo_count ()
        let foo_bump () = Bar_of_p_int.foo_bump ()
      end
    ]}

    [Wrap]'s cmi records [Bar[P:P_int]{Q}] in its bound_globals, and
    [Bar]'s records the runtime dependency [Stateful[P:P]].

    (b) [Bar]'s compiled unit is an instantiating functor over its
    runtime parameters — the [P] and [Q] argument blocks plus
    [Stateful[P:P]]'s main module block — roughly:

    {[
      (setglobal Bar!
        (makeblock 0
          (function P Q Stateful -> (makeblock 0 foo_count foo_bump))))
    ]}

    (c) Functorizing [Wrap] produces [Make : functor (Q) () ->
    Intf(Q).S].  Inside [Make], [Bar]'s [Stateful[P:P]] runtime slot
    (the [Rp_main_module_block] path in [bind_local_instance]) is
    substituted via [visible_arg_map = {P:P_int}] to become
    [Stateful[P:P_int]] — now complete, so it resolves to a
    [Pgetglobal] of the pre-instantiated compilation unit rather than
    a fresh binding inside [Make]:

    {[
      (function Q ()
        (let (bar = (apply (field 0 (global Bar!))
                       (global P_int!) Q
                       (global Stateful-P_int!)))  ; shared instance
          (makeblock 0 (* Wrap *) ...)))
    ]}

    Hence the bundle and the directly-instantiated [Static] in
    [main_nested.ml] observe one shared counter. *)

 readonly_files = "\
   stateful.mli stateful.ml \
   bar.mli bar.ml \
   wrap.mli wrap.ml \
   q_int.mli q_int.ml \
   main_nested.ml test_nested.reference \
 ";

 setup-ocamlc.byte-build-env;

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

 set flg_base = "-w -53";
 set flg = "$flg_base -no-alias-deps -nocwd";
 set flg_int_iface = "$flg -w -49";

 (* dune does not pass [-nocwd] to link *)
 set flg_link = "$flg_base -no-alias-deps";

 (* Step 1: parameters [P] and [Q], and argument [P_int]. *)

 flags = "$flg_int_iface";
 module = "p/p__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -H p -open-cmi p/p__.cmi";
 module = "p/p.mli";
 ocamlc.byte;

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for P -I p -H p_int -open-cmi p_int/p_int__.cmi";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlc.byte;

 flags = "$flg_int_iface";
 module = "q/q__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -H q -open-cmi q/q__.cmi";
 module = "q/q.mli";
 ocamlc.byte;

 flags = "$flg -as-argument-for Q -I q -I q_int";
 module = "q_int/q_int.mli q_int/q_int.ml";
 ocamlc.byte;

 (* Step 2: build [Stateful], parameterised by [P] — has a module-level
    counter. *)

 flags = "$flg -parameter P -I p -I stateful";
 module = "stateful/stateful.mli stateful/stateful.ml";
 ocamlc.byte;

 (* Step 3: pre-instantiate [Stateful[P:P_int]] into [instances/]. *)

 flags = "$flg -I p -I p_int -I stateful -instantiate";
 module = "";
 program = "instances/stateful-P_int.cmo";
 all_modules = "stateful/stateful.cmo p_int/p_int.cmo";
 ocamlc.byte;

 (* Step 4: build [Bar], parameterised by [P] and [Q].  Its body
    references [Stateful], so [Stateful] becomes a runtime parameter of
    [Bar]. *)

 flags = "$flg -parameter P -parameter Q -I p -I q -I stateful -I bar";
 module = "bar/bar.mli bar/bar.ml";
 ocamlc.byte;

 (* Step 5: build [Wrap] (parameterised by [Q] only) whose body
    references [Bar(P)(P_int)] — i.e. the compound
    [Bar[P:P_int]{Q}]. *)

 flags = "$flg -parameter Q -I p -I p_int -I q -I stateful -I bar -I wrap";
 module = "wrap/wrap.mli wrap/wrap.ml";
 ocamlc.byte;

 (* Step 6: functorize [Wrap].  Bar's [Rp_main_module_block Stateful[P:P]]
    is substituted to [Stateful[P:P_int]] (complete) and resolved to a
    [Pgetglobal] of the pre-instantiated CU. *)

 flags = "$flg -functorize -I p -I p_int -I q -I stateful -I instances \
   -I bar -I wrap Wrap";
 module = "";
 program = "bundle_wrap/bundle.cmo";
 all_modules = "";
 ocamlc.byte;

 (* Step 7: main verifies that [Stateful[P:P_int]]'s counter is shared
    between direct access via [Static] and access via the bundle. *)

 flags = "$flg -I bundle_wrap -I p -I p_int -I q -I q_int -I stateful \
   -I instances -I bar -I wrap";
 module = "main_nested.ml";
 ocamlc.byte;

 flags = "$flg_link";
 module = "";
 program = "$test_build_directory/test_nested.bc";
 all_modules = "\
   p_int/p_int__.cmo \
   p_int/p_int.cmo \
   stateful/stateful.cmo \
   instances/stateful-P_int.cmo \
   q_int/q_int.cmo \
   bar/bar.cmo \
   wrap/wrap.cmo \
   bundle_wrap/bundle.cmo \
   main_nested.cmo \
 ";
 ocamlc.byte;

 stdout = "test_nested.output";
 stderr = "test_nested.output";
 output = "test_nested.output";
 run;

 reference = "test_nested.reference";
 check-program-output;
*)

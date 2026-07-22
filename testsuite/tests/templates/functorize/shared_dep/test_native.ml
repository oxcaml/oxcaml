(* TEST (* DO NOT EDIT. Instead edit shared_dep/test_byte.ml and run gen-native.sh. *)
 (* Within a single [Bundle.Make (P_int) ()] application, [Stateful[P:P]]
    (a non-complete transitive dependency of both [Bar1] and [Bar2]) is
    [bind_local_instance]'d once and shared via the [module_map] cache
    — [Bar1] and [Bar2] see the same counter. *)

 readonly_files = "\
   stateful.mli stateful.ml \
   bar1.mli bar1.ml \
   bar2.mli bar2.ml \
   main_shared.ml test_shared.reference \
 ";

 setup-ocamlopt.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p p_int stateful bar1 bar2 bundle_shared";
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

 src = "stateful.mli stateful.ml"; dst = "stateful/"; copy;
 src = "bar1.mli bar1.ml";         dst = "bar1/";     copy;
 src = "bar2.mli bar2.ml";         dst = "bar2/";     copy;

 set flg = "-no-alias-deps -w -53";
 set flg_int_iface = "$flg -w -49";

 (* Step 1: parameter [P] and argument [P_int]. *)

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

 (* Step 2: build [Stateful], parameterised by [P] — has a module-level
    counter. *)

 flags = "$flg -parameter P -I p -I stateful";
 module = "stateful/stateful.mli stateful/stateful.ml";
 ocamlopt.byte;

 (* Step 3: build [Bar1] and [Bar2] — both parameterised by [P], both
    depend on [Stateful] (which becomes a runtime parameter of each). *)

 flags = "$flg -parameter P -I p -I stateful -I bar1";
 module = "bar1/bar1.mli bar1/bar1.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -I stateful -I bar2";
 module = "bar2/bar2.mli bar2/bar2.ml";
 ocamlopt.byte;

 (* Step 4: functorize [Bar1] and [Bar2] together.  [Stateful[P:P]] is
    a transitive dependency of both; the [module_map] cache ensures it
    is bound once inside the bundle's [Make] and shared. *)

 flags = "$flg -functorize -I p -I stateful -I bar1 -I bar2 Bar1 Bar2";
 module = "";
 program = "bundle_shared/bundle.cmx";
 all_modules = "";
 ocamlopt.byte;

 (* Step 5: verify that [Bar1] and [Bar2] observe the same shared
    counter. *)

 flags = "$flg -I bundle_shared -I p -I p_int -I stateful -I bar1 -I bar2";
 module = "main_shared.ml";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_shared.exe";
 all_modules = "\
   p_int/p_int__.cmx \
   p_int/p_int.cmx \
   stateful/stateful.cmx \
   bar1/bar1.cmx \
   bar2/bar2.cmx \
   bundle_shared/bundle.cmx \
   main_shared.cmx \
 ";
 ocamlopt.byte;

 stdout = "test_shared.output";
 stderr = "test_shared.output";
 output = "test_shared.output";
 run;

 reference = "test_shared.reference";
 check-program-output;
*)

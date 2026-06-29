(* TEST (* DO NOT EDIT. Instead edit simple/test_byte.ml and run gen-native.sh. *)
 (* Bundle [Basic] + [Util] (both [-parameter P]) and check three
    consumer patterns:

    1. Direct application: [Bundle.Make(P_int)()] — [.cmi]+[.cmx]
       save/load round-trip.
    2. Intf sharing: ascribe with [Bundle.Intf(P_int).S] — checks
       module-type sharing between the [Intf] and [Make] functors.
    3. Double application: two applications of [Bundle.Make] yield
       fresh abstract types. *)

 readonly_files = "\
   main_functorize.ml main_functorize_intf_sig.ml main_functorize_double.ml \
   test_functorize.reference test_functorize_double.reference \
 ";

 setup-ocamlopt.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p basic util p_int bundle";
 script;

 src = "${test_source_directory}/../p.mli \
        ${test_source_directory}/../../dunelike/p__.ml";
 dst = "p/";
 copy;

 src = "${test_source_directory}/../../dunelike/basic.mli \
        ${test_source_directory}/../../dunelike/basic.ml \
        ${test_source_directory}/../../dunelike/basic__.ml";
 dst = "basic/";
 copy;

 src = "${test_source_directory}/../../dunelike/util.mli \
        ${test_source_directory}/../../dunelike/util.ml \
        ${test_source_directory}/../../dunelike/util__.ml";
 dst = "util/";
 copy;

 src = "${test_source_directory}/../../dunelike/p_int.mli \
        ${test_source_directory}/../../dunelike/p_int.ml \
        ${test_source_directory}/../../dunelike/p_int__.ml";
 dst = "p_int/";
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

 (* [Basic] and [Util], both parameterised by P. *)

 flags = "$flg_int_iface -parameter P -I p";
 module = "basic/basic__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -I basic -open Basic__";
 module = "basic/basic.mli basic/basic.ml";
 ocamlopt.byte;

 flags = "$flg_int_iface -parameter P -I p";
 module = "util/util__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -I util -open Util__";
 module = "util/util.mli util/util.ml";
 ocamlopt.byte;

 (* [P_int] argument for P. *)

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlopt.byte;

 flags = "$flg -as-argument-for P -I p -I p_int -open P_int__";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlopt.byte;

 (* Bundle [Basic] and [Util]. *)

 flags = "$flg -functorize -I p -I basic -I util Basic Util";
 module = "";
 program = "bundle/bundle.cmx";
 all_modules = "";
 ocamlopt.byte;

 (* (1) Direct application of [Bundle.Make(P_int)()]. *)

 flags = "$flg -I bundle -I p -I p_int -I basic -I util";
 module = "main_functorize.ml";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize.exe";
 all_modules = "\
   basic/basic__.cmx \
   util/util__.cmx \
   basic/basic.cmx \
   util/util.cmx \
   p_int/p_int__.cmx \
   p_int/p_int.cmx \
   bundle/bundle.cmx \
   main_functorize.cmx \
 ";
 ocamlopt.byte;

 stdout = "test_functorize.output";
 stderr = "test_functorize.output";
 output = "test_functorize.output";
 run;

 reference = "test_functorize.reference";
 check-program-output;

 (* (2) Module-type sharing: [Bundle.Make(P_int)()] satisfies
    [Bundle.Intf(P_int).S]. *)

 flags = "$flg -I bundle -I p -I p_int -I basic -I util";
 module = "main_functorize_intf_sig.ml";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize_intf_sig.exe";
 all_modules = "\
   basic/basic__.cmx \
   util/util__.cmx \
   basic/basic.cmx \
   util/util.cmx \
   p_int/p_int__.cmx \
   p_int/p_int.cmx \
   bundle/bundle.cmx \
   main_functorize_intf_sig.cmx \
 ";
 ocamlopt.byte;

 stdout = "test_functorize_intf_sig.output";
 stderr = "test_functorize_intf_sig.output";
 output = "test_functorize_intf_sig.output";
 run;

 reference = "test_functorize.reference";
 check-program-output;

 (* (3) Double application: two applications of [Bundle.Make] yield fresh
    abstract types. *)

 flags = "$flg -I bundle -I p -I p_int -I basic -I util";
 module = "main_functorize_double.ml";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize_double.exe";
 all_modules = "\
   basic/basic__.cmx \
   util/util__.cmx \
   basic/basic.cmx \
   util/util.cmx \
   p_int/p_int__.cmx \
   p_int/p_int.cmx \
   bundle/bundle.cmx \
   main_functorize_double.cmx \
 ";
 ocamlopt.byte;

 stdout = "test_functorize_double.output";
 stderr = "test_functorize_double.output";
 output = "test_functorize_double.output";
 run;

 reference = "test_functorize_double.reference";
 check-program-output;
*)

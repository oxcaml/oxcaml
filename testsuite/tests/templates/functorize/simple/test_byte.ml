(* TEST
 (* Basic functorize: bundle [Basic] and [Util] (both parameterised by P)
    into a single functor [Bundle.Make] and verify three consumer patterns:

    1. [main_functorize.ml]: direct application of [Bundle.Make(P_int)()]
       — checks the [.cmi]+[.cmo] save/load round-trip and runtime layout.

    2. [main_functorize_intf_sig.ml]: ascribe the result with
       [Bundle.Intf(P_int).S] — checks module-type sharing between the
       applicative [Intf] functor and the generative [Make] functor.

    3. [main_functorize_double.ml]: apply [Bundle.Make] twice — checks that
       each application yields fresh abstract types so values from R1 and R2
       can't be mixed at the type level (though the test only exercises
       independent uses of each application). *)

 readonly_files = "\
   main_functorize.ml main_functorize_intf_sig.ml main_functorize_double.ml \
   test_functorize.reference test_functorize_double.reference \
 ";

 setup-ocamlc.byte-build-env;

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
 ocamlc.byte;

 flags = "$flg -as-parameter -I p -open P__";
 module = "p/p.mli";
 ocamlc.byte;

 (* [Basic] and [Util], both parameterised by P. *)

 flags = "$flg_int_iface -parameter P -I p";
 module = "basic/basic__.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -I p -I basic -open Basic__";
 module = "basic/basic.mli basic/basic.ml";
 ocamlc.byte;

 flags = "$flg_int_iface -parameter P -I p";
 module = "util/util__.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -I p -I util -open Util__";
 module = "util/util.mli util/util.ml";
 ocamlc.byte;

 (* [P_int] argument for P. *)

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for P -I p -I p_int -open P_int__";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlc.byte;

 (* Bundle [Basic] and [Util]. *)

 flags = "$flg -functorize -I p -I basic -I util Basic Util";
 module = "";
 program = "bundle/bundle.cmo";
 all_modules = "";
 ocamlc.byte;

 (* (1) Direct application of [Bundle.Make(P_int)()]. *)

 flags = "$flg -I bundle -I p -I p_int -I basic -I util";
 module = "main_functorize.ml";
 ocamlc.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize.bc";
 all_modules = "\
   basic/basic__.cmo \
   util/util__.cmo \
   basic/basic.cmo \
   util/util.cmo \
   p_int/p_int__.cmo \
   p_int/p_int.cmo \
   bundle/bundle.cmo \
   main_functorize.cmo \
 ";
 ocamlc.byte;

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
 ocamlc.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize_intf_sig.bc";
 all_modules = "\
   basic/basic__.cmo \
   util/util__.cmo \
   basic/basic.cmo \
   util/util.cmo \
   p_int/p_int__.cmo \
   p_int/p_int.cmo \
   bundle/bundle.cmo \
   main_functorize_intf_sig.cmo \
 ";
 ocamlc.byte;

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
 ocamlc.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize_double.bc";
 all_modules = "\
   basic/basic__.cmo \
   util/util__.cmo \
   basic/basic.cmo \
   util/util.cmo \
   p_int/p_int__.cmo \
   p_int/p_int.cmo \
   bundle/bundle.cmo \
   main_functorize_double.cmo \
 ";
 ocamlc.byte;

 stdout = "test_functorize_double.output";
 stderr = "test_functorize_double.output";
 output = "test_functorize_double.output";
 run;

 reference = "test_functorize_double.reference";
 check-program-output;
*)

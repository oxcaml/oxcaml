(* TEST (* DO NOT EDIT. Instead edit cmifile/test_byte.ml and run gen-native.sh. *)
 (* The [-cmi-file] flag: tell [-functorize] to use a pre-existing [.cmi]
    rather than generating one — useful for splitting the bundle into a
    [.cmi]-only declaration phase and a [.cmx]-only body phase, or for
    asserting that the bundle matches a user-supplied interface.

    1. Positive: two-phase compilation with [-cmi-file] is
       runtime-equivalent to the one-step [.cmx] target — phase 1 generates
       the bundle [.cmi], phase 2 generates the [.cmx] against that
       pre-existing [.cmi] (no [.cmi] regenerated in phase 2).

    2. Negative ([bad_cmi_file_struct]): the [-cmi-file] declares [Make] as
       a plain structure, but the bundle infers a functor.  Inclusion check
       rejects. *)

 readonly_files = "\
   main_functorize.ml test_functorize.reference \
   bundle_bad.mli \
   bad_cmi_file_struct.reference \
 ";

 setup-ocamlopt.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p basic util p_int \
                 bundle_cmifile bundle_bad";
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

 src = "bundle_bad.mli";
 dst = "bundle_bad/";
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

 (* [Basic] and [Util]. *)

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

 (* (1) Positive: phase 1 generates the bundle [.cmi]. *)

 flags = "$flg -functorize -I p -I basic -I util Basic Util";
 module = "";
 program = "bundle_cmifile/bundle.cmi";
 all_modules = "";
 ocamlopt.byte;

 (* Phase 2: generate the [.cmx] against the [.cmi] from phase 1.  With
    [-cmi-file], no [.cmi] is (re)written by this invocation. *)

 flags = "$flg -functorize -I p -I basic -I util \
   -cmi-file bundle_cmifile/bundle.cmi Basic Util";
 module = "";
 program = "bundle_cmifile/bundle.cmx";
 all_modules = "";
 ocamlopt.byte;

 (* Verify the two-phase bundle is runtime-equivalent to the one-step
    bundle: the consumer program ([main_functorize.ml]) is the one used by
    [basic_util], and the reference is the same [test_functorize.reference]. *)

 flags = "$flg -I bundle_cmifile -I p -I p_int -I basic -I util";
 module = "main_functorize.ml";
 ocamlopt.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize_cmifile.exe";
 all_modules = "\
   basic/basic__.cmx \
   util/util__.cmx \
   basic/basic.cmx \
   util/util.cmx \
   p_int/p_int__.cmx \
   p_int/p_int.cmx \
   bundle_cmifile/bundle.cmx \
   main_functorize.cmx \
 ";
 ocamlopt.byte;

 stdout = "test_functorize_cmifile.output";
 stderr = "test_functorize_cmifile.output";
 output = "test_functorize_cmifile.output";
 run;

 reference = "test_functorize.reference";
 check-program-output;

 (* Compile [bundle_bad.mli] — declares [Make] as a plain structure. *)

 flags = "";
 module = "bundle_bad/bundle_bad.mli";
 ocamlopt.byte;

 (* (2) Negative: declared cmi has [Make] as a struct; the bundle infers
    a functor.  Inclusion check rejects. *)

 flags = "$flg -functorize -I p -I basic -I util \
   -cmi-file bundle_bad/bundle_bad.cmi Basic Util";
 module = "";
 program = "bundle_bad/bundle_bad.cmx";
 all_modules = "";
 ocamlopt_byte_exit_status = "2";
 compiler_output = "bad_cmi_file_struct.output";
 ocamlopt.byte;

 compiler_reference = "bad_cmi_file_struct.reference";
 check-ocamlopt.byte-output;
*)

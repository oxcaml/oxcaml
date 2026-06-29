(* TEST
 (* Link-time CRC mismatch on the bundle's own cmi: generate
    [bundle_crc.cmi] v1 (Basic+Util), then [bundle_crc.cmo] against v1,
    then overwrite the cmi with v2 (Basic only), compile the consumer
    against v2, and link — the linker should catch the [Bundle_crc]
    CRC inconsistency. *)

 readonly_files = "\
   bad_bundle_cmi_overwritten_byte.reference \
   main_crc_mismatch.ml \
 ";

 setup-ocamlc.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p basic util p_int bundle_crc";
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

 (* [P_int] is an argument for P. *)

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for P -I p -I p_int -open P_int__";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlc.byte;

 (* Phase 1: generate [Bundle_crc.cmi] with [Basic+Util]. *)

 flags = "$flg -functorize -I p -I basic -I util Basic Util";
 module = "";
 program = "bundle_crc/bundle_crc.cmi";
 all_modules = "";
 ocamlc.byte;

 (* Phase 2: produce [Bundle_crc.cmo] recording the v1 cmi CRC. *)

 flags = "$flg -functorize -I p -I basic -I util \
   -cmi-file bundle_crc/bundle_crc.cmi Basic Util";
 module = "";
 program = "bundle_crc/bundle_crc.cmo";
 all_modules = "";
 ocamlc.byte;

 (* Overwrite [Bundle_crc.cmi] with a different signature (Basic only). *)

 flags = "$flg -functorize -I p -I basic Basic";
 module = "";
 program = "bundle_crc/bundle_crc.cmi";
 all_modules = "";
 ocamlc.byte;

 (* Compile [main_crc_mismatch.ml] against the v2 cmi. *)

 flags = "$flg -I bundle_crc -I p -I p_int -I basic";
 module = "main_crc_mismatch.ml";
 ocamlc.byte;

 (* Link: [bundle_crc.cmo] recorded CRC of cmi v1; [main_crc_mismatch.cmo]
    recorded CRC of cmi v2.  The linker must detect the inconsistency. *)
 flags = "";
 module = "";
 program = "$test_build_directory/test_crc_mismatch.bc";
 all_modules = "\
   basic/basic__.cmo \
   util/util__.cmo \
   basic/basic.cmo \
   util/util.cmo \
   p_int/p_int__.cmo \
   p_int/p_int.cmo \
   bundle_crc/bundle_crc.cmo \
   main_crc_mismatch.cmo \
 ";
 ocamlc_byte_exit_status = "2";
 compiler_output = "bad_bundle_cmi_overwritten.output";
 ocamlc.byte;

 compiler_reference = "bad_bundle_cmi_overwritten_byte.reference";
 check-ocamlc.byte-output;
*)

(* TEST (* DO NOT EDIT. Instead edit bad_bundle_cmi_overwritten/test_byte.ml and run gen-native.sh. *)
 (* Link-time CRC mismatch on the bundle's own cmi: generate
    [bundle_crc.cmi] v1 (Basic+Util), then [bundle_crc.cmx] against v1,
    then overwrite the cmi with v2 (Basic only), compile the consumer
    against v2, and link — the linker should catch the [Bundle_crc]
    CRC inconsistency. *)

 readonly_files = "\
   bad_bundle_cmi_overwritten_native.reference \
   main_crc_mismatch.ml \
 ";

 setup-ocamlopt.byte-build-env;

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

 set flg_base = "-w -53";
 set flg = "$flg_base -no-alias-deps -nocwd";
 set flg_int_iface = "$flg -w -49";

 (* dune does not pass [-nocwd] to link *)
 set flg_link = "$flg_base -no-alias-deps";

 (* Parameter P. *)

 flags = "$flg_int_iface";
 module = "p/p__.ml";
 ocamlopt.byte;

 flags = "$flg -as-parameter -H p -open-cmi p/p__.cmi";
 module = "p/p.mli";
 ocamlopt.byte;

 (* [Basic] and [Util], both parameterised by P. *)

 flags = "$flg_int_iface -parameter P -I p";
 module = "basic/basic__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -H basic -open-cmi basic/basic__.cmi";
 module = "basic/basic.mli basic/basic.ml";
 ocamlopt.byte;

 flags = "$flg_int_iface -parameter P -I p";
 module = "util/util__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -H util -open-cmi util/util__.cmi";
 module = "util/util.mli util/util.ml";
 ocamlopt.byte;

 (* [P_int] is an argument for P. *)

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlopt.byte;

 flags = "$flg -as-argument-for P -I p -H p_int -open-cmi p_int/p_int__.cmi";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlopt.byte;

 (* Phase 1: generate [Bundle_crc.cmi] with [Basic+Util]. *)

 flags = "$flg -functorize -I p -I basic -I util Basic Util";
 module = "";
 program = "bundle_crc/bundle_crc.cmi";
 all_modules = "";
 ocamlopt.byte;

 (* Phase 2: produce [Bundle_crc.cmx] recording the v1 cmi CRC. *)

 flags = "$flg -functorize -I p -I basic -I util \
   -cmi-file bundle_crc/bundle_crc.cmi Basic Util";
 module = "";
 program = "bundle_crc/bundle_crc.cmx";
 all_modules = "";
 ocamlopt.byte;

 (* Overwrite [Bundle_crc.cmi] with a different signature (Basic only). *)

 flags = "$flg -functorize -I p -I basic Basic";
 module = "";
 program = "bundle_crc/bundle_crc.cmi";
 all_modules = "";
 ocamlopt.byte;

 (* Compile [main_crc_mismatch.ml] against the v2 cmi. *)

 flags = "$flg -I bundle_crc -I p -I p_int -I basic";
 module = "main_crc_mismatch.ml";
 ocamlopt.byte;

 (* Link: [bundle_crc.cmx] recorded CRC of cmi v1; [main_crc_mismatch.cmx]
    recorded CRC of cmi v2.  The linker must detect the inconsistency. *)
 flags = "$flg_link";
 module = "";
 program = "$test_build_directory/test_crc_mismatch.exe";
 all_modules = "\
   basic/basic__.cmx \
   util/util__.cmx \
   basic/basic.cmx \
   util/util.cmx \
   p_int/p_int__.cmx \
   p_int/p_int.cmx \
   bundle_crc/bundle_crc.cmx \
   main_crc_mismatch.cmx \
 ";
 ocamlopt_byte_exit_status = "2";
 compiler_output = "bad_bundle_cmi_overwritten.output";
 ocamlopt.byte;

 compiler_reference = "bad_bundle_cmi_overwritten_native.reference";
 check-ocamlopt.byte-output;
*)

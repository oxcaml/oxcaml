(* TEST (* DO NOT EDIT. Instead edit crc_mismatch/test_byte.ml and run gen-native.sh. *)
 (* CRC mismatches involving [-functorize] bundles:

    1. Bundle's own cmi overwritten between the two [-cmi-file] phases:
       the linker catches the inconsistency.
    2. Dep cmi changed: [basic_pq__.cmi] is recompiled with fewer
       parameters after [User_pq] was compiled against the older CRC —
       Consistbl catches it at functorize time. *)

 readonly_files = "\
   main_crc_mismatch.ml \
   bad_bundle_cmi_overwritten_native.reference \
   basic_pq.mli basic_pq.ml basic_pq__.ml \
   user_pq.mli user_pq.ml user_pq__.ml \
   bad_dep_cmi_changed.reference \
 ";

 setup-ocamlopt.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p q p_int basic util bundle_crc bundle";
 script;

 src = "${test_source_directory}/../p.mli \
        ${test_source_directory}/../../dunelike/p__.ml";
 dst = "p/";
 copy;

 src = "${test_source_directory}/../../dunelike/q.mli \
        ${test_source_directory}/../../dunelike/q__.ml";
 dst = "q/";
 copy;

 src = "${test_source_directory}/../../dunelike/p_int.mli \
        ${test_source_directory}/../../dunelike/p_int.ml \
        ${test_source_directory}/../../dunelike/p_int__.ml";
 dst = "p_int/";
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

 {
   (* ===== Case 1: bundle cmi overwritten between phases ===== *)

   (* Link-time CRC mismatch on the bundle's own cmi: generate
      [bundle_crc.cmi] v1 (Basic+Util), then [bundle_crc.cmx] against
      v1, then overwrite the cmi with v2 (Basic only), compile the
      consumer against v2, and link — the linker should catch the
      [Bundle_crc] CRC inconsistency. *)

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

   flags = "$flg_int_iface";
   module = "p_int/p_int__.ml";
   ocamlopt.byte;

   flags = "$flg -as-argument-for P -I p -H p_int -open-cmi p_int/p_int__.cmi";
   module = "p_int/p_int.mli p_int/p_int.ml";
   ocamlopt.byte;

   flags = "$flg -functorize -I p -I basic -I util Basic Util";
   module = "";
   program = "bundle_crc/bundle_crc.cmi";
   all_modules = "";
   ocamlopt.byte;

   flags = "$flg -functorize -I p -I basic -I util \
     -cmi-file bundle_crc/bundle_crc.cmi Basic Util";
   module = "";
   program = "bundle_crc/bundle_crc.cmx";
   all_modules = "";
   ocamlopt.byte;

   flags = "$flg -functorize -I p -I basic Basic";
   module = "";
   program = "bundle_crc/bundle_crc.cmi";
   all_modules = "";
   ocamlopt.byte;

   flags = "$flg -I bundle_crc -I p -I p_int -I basic";
   module = "main_crc_mismatch.ml";
   ocamlopt.byte;

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
 }{
   (* ===== Case 2: dep cmi changed ===== *)

   (* Parameter Q. *)

   flags = "$flg_int_iface";
   module = "q/q__.ml";
   ocamlopt.byte;

   flags = "$flg -as-parameter -H q -open-cmi q/q__.cmi";
   module = "q/q.mli";
   ocamlopt.byte;

   (* [Basic_pq] is compiled with -parameter P -parameter Q first. *)

   flags = "$flg_int_iface -parameter P -parameter Q -I p -I q";
   module = "basic_pq__.ml";
   ocamlopt.byte;

   flags = "$flg -parameter P -parameter Q -I . -I p -I q \
     -open-cmi basic_pq__.cmi";
   module = "basic_pq.mli basic_pq.ml";
   ocamlopt.byte;

   (* [User_pq] references [Basic_pq] from a -parameter P -parameter Q
      context — records [Basic_pq__]'s CRC v1. *)

   flags = "$flg_int_iface -parameter P -parameter Q -I p -I q";
   module = "user_pq__.ml";
   ocamlopt.byte;

   flags = "$flg -parameter P -parameter Q -I . -I p -I q \
     -open-cmi user_pq__.cmi";
   module = "user_pq.mli user_pq.ml";
   ocamlopt.byte;

   (* Recompile [Basic_pq] with only -parameter P, overwriting [basic_pq__]'s
      cmi (v2 with a different CRC). *)

   flags = "$flg_int_iface -parameter P -I p";
   module = "basic_pq__.ml";
   ocamlopt.byte;

   flags = "$flg -parameter P -I . -I p -open-cmi basic_pq__.cmi";
   module = "basic_pq.mli basic_pq.ml";
   ocamlopt.byte;

   flags = "$flg -functorize -I . -I p -I q User_pq";
   module = "";
   program = "bundle/bundle.cmi";
   all_modules = "";
   ocamlopt_byte_exit_status = "2";
   compiler_output = "bad_dep_cmi_changed.output";
   ocamlopt.byte;

   compiler_reference = "bad_dep_cmi_changed.reference";
   check-ocamlopt.byte-output;
 }
*)

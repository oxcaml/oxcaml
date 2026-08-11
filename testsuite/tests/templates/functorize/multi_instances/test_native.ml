(* TEST (* DO NOT EDIT. Instead edit multi_instances/test_byte.ml and run gen-native.sh. *)
 (* Type identity across bundle applications. *)

 readonly_files = "\
   basic_opaque.mli basic_opaque.ml util_opaque.mli util_opaque.ml \
   basic_transparent.ml util_transparent.ml \
   basic_pfree.mli basic_pfree.ml \
   p_int_alt.mli p_int_alt.ml \
   main_functorize_share.ml test_functorize_share.reference \
   main_functorize_type_share.ml test_functorize_type_share.reference \
   main_instance_state.ml test_instance_state.reference \
   main_intf_sig.ml test_intf_sig.reference \
   bad_intf_sig_mismatch.ml bad_intf_sig_mismatch.reference \
   bad_mix_share.ml \
 ";

 setup-ocamlopt.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p p_int p_int_alt basic_opaque util_opaque \
                 basic_transparent util_transparent \
                 basic_pfree bundle_opaque bundle2 bundle_pfree";
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

 src = "basic_opaque.mli basic_opaque.ml";
 dst = "basic_opaque/";
 copy;

 src = "util_opaque.mli util_opaque.ml";
 dst = "util_opaque/";
 copy;

 src = "basic_transparent.ml";
 dst = "basic_transparent/";
 copy;

 src = "util_transparent.ml";
 dst = "util_transparent/";
 copy;

 src = "basic_pfree.mli basic_pfree.ml";
 dst = "basic_pfree/";
 copy;

 src = "p_int_alt.mli p_int_alt.ml";
 dst = "p_int_alt/";
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

 (* [P_int] argument for P. *)

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlopt.byte;

 flags = "$flg -as-argument-for P -I p -H p_int -open-cmi p_int/p_int__.cmi";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlopt.byte;

 (* Second argument for P, distinct identity from [P_int]. *)

 flags = "$flg -as-argument-for P -I p -I p_int_alt";
 module = "p_int_alt/p_int_alt.mli p_int_alt/p_int_alt.ml";
 ocamlopt.byte;

 (* [Basic_pfree]: P-parameterised, P-free sig. *)

 flags = "$flg -parameter P -I p -I basic_pfree";
 module = "basic_pfree/basic_pfree.mli basic_pfree/basic_pfree.ml";
 ocamlopt.byte;

 (* [Basic_opaque] (abstract [t] with counter). *)

 flags = "$flg -parameter P -I p -I basic_opaque";
 module = "basic_opaque/basic_opaque.mli basic_opaque/basic_opaque.ml";
 ocamlopt.byte;

 (* [Util_opaque]: [type t = Basic_opaque.t]. *)

 flags = "$flg -parameter P -I p -I basic_opaque -I util_opaque";
 module = "util_opaque/util_opaque.mli util_opaque/util_opaque.ml";
 ocamlopt.byte;

 (* [Basic_transparent] and [Util_transparent]: transparent [type t = P.t]. *)

 flags = "$flg -parameter P -I p";
 module = "basic_transparent/basic_transparent.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p";
 module = "util_transparent/util_transparent.ml";
 ocamlopt.byte;

 (* Bundle the two flavours. *)

 flags = "$flg -functorize -I p -I basic_opaque -I util_opaque \
   Basic_opaque Util_opaque";
 module = "";
 program = "bundle_opaque/bundle_opaque.cmx";
 all_modules = "";
 ocamlopt.byte;

 flags = "$flg -functorize -I p -I basic_transparent -I util_transparent \
   Basic_transparent Util_transparent";
 module = "";
 program = "bundle2/bundle2.cmx";
 all_modules = "";
 ocamlopt.byte;

 flags = "$flg -functorize -I p -I basic_pfree Basic_pfree";
 module = "";
 program = "bundle_pfree/bundle_pfree.cmx";
 all_modules = "";
 ocamlopt.byte;

 {
   (* (1) Positive: transparent sharing — within one [R = Make(P_int)()]
      and also across [R1 = Make(P_int)()], [R2 = Make(P_int)()]. *)

   flags = "$flg -I bundle2 -I p -I p_int -I basic_transparent \
     -I util_transparent";
   module = "main_functorize_share.ml";
   ocamlopt.byte;

   flags = "$flg_link";
   module = "";
   program = "$test_build_directory/test_functorize_share.exe";
   all_modules = "\
     basic_transparent/basic_transparent.cmx \
     util_transparent/util_transparent.cmx \
     p_int/p_int__.cmx \
     p_int/p_int.cmx \
     bundle2/bundle2.cmx \
     main_functorize_share.cmx \
   ";
   ocamlopt.byte;

   stdout = "test_functorize_share.output";
   stderr = "test_functorize_share.output";
   output = "test_functorize_share.output";
   run;

   reference = "test_functorize_share.reference";
   check-program-output;
 }{
   (* (2) Positive: abstract+eq sharing — within one [R = Make(P_int)()]. *)

   flags = "$flg -I bundle_opaque -I p -I p_int -I basic_opaque -I util_opaque";
   module = "main_functorize_type_share.ml";
   ocamlopt.byte;

   flags = "$flg_link";
   module = "";
   program = "$test_build_directory/test_functorize_type_share.exe";
   all_modules = "\
     basic_opaque/basic_opaque.cmx \
     util_opaque/util_opaque.cmx \
     p_int/p_int__.cmx \
     p_int/p_int.cmx \
     bundle_opaque/bundle_opaque.cmx \
     main_functorize_type_share.cmx \
   ";
   ocamlopt.byte;

   stdout = "test_functorize_type_share.output";
   stderr = "test_functorize_type_share.output";
   output = "test_functorize_type_share.output";
   run;

   reference = "test_functorize_type_share.reference";
   check-program-output;
 }{
   (* (3) Instance independence — each [Make] application has its own
      counter. *)

   flags = "$flg -I bundle_opaque -I p -I p_int -I basic_opaque -I util_opaque";
   module = "main_instance_state.ml";
   ocamlopt.byte;

   flags = "$flg_link";
   module = "";
   program = "$test_build_directory/test_instance_state.exe";
   all_modules = "\
     basic_opaque/basic_opaque.cmx \
     util_opaque/util_opaque.cmx \
     p_int/p_int__.cmx \
     p_int/p_int.cmx \
     bundle_opaque/bundle_opaque.cmx \
     main_instance_state.cmx \
   ";
   ocamlopt.byte;

   stdout = "test_instance_state.output";
   stderr = "test_instance_state.output";
   output = "test_instance_state.output";
   run;

   reference = "test_instance_state.reference";
   check-program-output;
 }{
   (* (4) [Make(M0)()] satisfies [Intf(M1).S] when the bundle sig is
      P-free. *)

   flags = "$flg -I bundle_pfree -I p -I p_int -I p_int_alt -I basic_pfree";
   module = "main_intf_sig.ml";
   ocamlopt.byte;

   flags = "$flg_link";
   module = "";
   program = "$test_build_directory/test_intf_sig.exe";
   all_modules = "\
     basic_pfree/basic_pfree.cmx \
     p_int/p_int__.cmx \
     p_int/p_int.cmx \
     p_int_alt/p_int_alt.cmx \
     bundle_pfree/bundle_pfree.cmx \
     main_intf_sig.cmx \
   ";
   ocamlopt.byte;

   stdout = "test_intf_sig.output";
   stderr = "test_intf_sig.output";
   output = "test_intf_sig.output";
   run;

   reference = "test_intf_sig.reference";
   check-program-output;
 }{
   (* (5) [Make(M0)()] does NOT satisfy [Intf(M1).S] when the bundle sig
      references [P] (here [Basic_transparent.t = P.t]). *)

   flags = "$flg -I bundle2 -I p -I p_int -I p_int_alt \
     -I basic_transparent -I util_transparent";
   module = "bad_intf_sig_mismatch.ml";
   ocamlopt_byte_exit_status = "2";
   compiler_output = "bad_intf_sig_mismatch.output";
   ocamlopt.byte;

   compiler_reference = "bad_intf_sig_mismatch.reference";
   check-ocamlopt.byte-output;
 }{
   (* (6) Negative: a value from [R1 = Make(P_int)()] cannot be used
      where an [R2 = Make(P_int)()] value is expected — each [Make]
      application gives fresh abstract types. *)

   flags = "$flg -I bundle_opaque -I p -I p_int -I basic_opaque -I util_opaque";
   module = "bad_mix_share.ml";
   ocamlopt_byte_exit_status = "2";
   ocamlopt.byte;
 }
*)

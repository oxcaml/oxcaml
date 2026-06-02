(* TEST (* DO NOT EDIT. Instead edit test_functorize_byte.ml and run gen-native.sh. *)
 readonly_files = "\
   p.mli p__.ml \
   basic.mli basic.ml basic__.ml \
   util.mli util.ml util__.ml \
   basic2.ml util2.ml \
   basic_share.mli basic_share.ml \
   util_share.mli util_share.ml \
   derived.mli derived.ml \
   p_int.mli p_int.ml p_int__.ml \
   main_functorize.ml \
   main_functorize_intf_sig.ml \
   main_functorize_double.ml \
   main_functorize_derived.ml \
   main_functorize_share.ml \
   main_functorize_type_share.ml \
   message.mli message.ml with_message.ml pure_alias.ml \
   main_pure_alias.ml \
   test_functorize_pure_alias.reference \
   plain.mli plain.ml uses_plain.ml main_uses_plain.ml \
   test_functorize_uses_plain.reference \
   bad_non_param_input.reference \
   bad_mix_bundles.ml \
   bad_mix_share.ml \
   test_functorize.reference \
   test_functorize_double.reference \
   test_functorize_derived.reference \
   test_functorize_share.reference \
   test_functorize_type_share.reference \
   bad_mix_bundles.reference \
   bundle_phases.cmi.objinfo.reference \
   bundle_cms.cms.objinfo_native.reference \
   bundle_bad.mli \
   bad_swap.reference \
   bad_cmi_file.reference \
   main_crc_mismatch.ml \
   bad_crc_mismatch_native.reference \
 ";
 {
   setup-ocamlopt.byte-build-env;

   set OCAMLPARAM = "";

   script = "mkdir p basic util basic2 util2 basic_share util_share derived p_int message with_message pure_alias plain uses_plain bundle bundle2 bundle_derived bundle_share bundle_msg bundle_pure_alias bundle_uses_plain bundle_bad_non_param bundle_stopafter bundle_cmifile bundle_phases bundle_onestep bundle_cms bundle_bad bundle_swap bundle_crc";
   script;

   src = "p.mli p__.ml";
   dst = "p/";
   copy;

   src = "basic.mli basic.ml basic__.ml";
   dst = "basic/";
   copy;

   src = "util.mli util.ml util__.ml";
   dst = "util/";
   copy;

   src = "basic2.ml";
   dst = "basic2/";
   copy;

   src = "util2.ml";
   dst = "util2/";
   copy;

   src = "basic_share.mli basic_share.ml";
   dst = "basic_share/";
   copy;

   src = "util_share.mli util_share.ml";
   dst = "util_share/";
   copy;

   src = "p_int.mli p_int.ml p_int__.ml";
   dst = "p_int/";
   copy;

   src = "message.mli message.ml";
   dst = "message/";
   copy;

   src = "with_message.ml";
   dst = "with_message/";
   copy;

   src = "pure_alias.ml";
   dst = "pure_alias/";
   copy;

   src = "plain.mli plain.ml";
   dst = "plain/";
   copy;

   src = "uses_plain.ml";
   dst = "uses_plain/";
   copy;

   src = "bundle_bad.mli";
   dst = "bundle_bad/";
   copy;

   set flg = "-no-alias-deps -nocwd -w -53";
   set flg_int_iface = "$flg -w -49";

   (* Parameter P *)

   flags = "$flg_int_iface";
   module = "p/p__.ml";
   ocamlopt.byte;

   flags = "$flg -as-parameter -I p -open P__";
   module = "p/p.mli";
   ocamlopt.byte;

   (* Basic, parameterized by P *)

   flags = "$flg_int_iface -parameter P -I p";
   module = "basic/basic__.ml";
   ocamlopt.byte;

   flags = "$flg -parameter P -I p -I basic -open Basic__";
   module = "basic/basic.mli basic/basic.ml";
   ocamlopt.byte;

   (* Util, parameterized by P *)

   flags = "$flg_int_iface -parameter P -I p";
   module = "util/util__.ml";
   ocamlopt.byte;

   flags = "$flg -parameter P -I p -I util -open Util__";
   module = "util/util.mli util/util.ml";
   ocamlopt.byte;

   (* P_int, argument for P *)

   flags = "$flg_int_iface";
   module = "p_int/p_int__.ml";
   ocamlopt.byte;

   flags = "$flg -as-argument-for P -I p -I p_int -open P_int__";
   module = "p_int/p_int.mli p_int/p_int.ml";
   ocamlopt.byte;

   (* Transparent modules: basic2 and util2 expose type t = P.t *)

   flags = "$flg -parameter P -I p";
   module = "basic2/basic2.ml";
   ocamlopt.byte;

   flags = "$flg -parameter P -I p";
   module = "util2/util2.ml";
   ocamlopt.byte;

   (* Derived: parameterized by P, depends on Basic *)

   src = "derived.mli derived.ml";
   dst = "derived/";
   copy;

   flags = "$flg -parameter P -I p -I basic -I derived";
   module = "derived/derived.mli derived/derived.ml";
   ocamlopt.byte;

   (* basic_share: abstract type t, with a counter *)

   flags = "$flg -parameter P -I p -I basic_share";
   module = "basic_share/basic_share.mli basic_share/basic_share.ml";
   ocamlopt.byte;

   (* util_share: type t = Basic_share.t, shares type with basic_share *)

   flags = "$flg -parameter P -I p -I basic_share -I util_share";
   module = "util_share/util_share.mli util_share/util_share.ml";
   ocamlopt.byte;

   (* Bundle basic and util together into a single functor *)

   flags = "$flg -functorize -I p -I basic -I util";
   module = "";
   program = "bundle/bundle.cmx";
   all_modules = "basic/basic.cmx util/util.cmx";
   ocamlopt.byte;

   (* Bundle basic and derived: derived depends on basic *)

   flags = "$flg -functorize -I p -I basic -I derived";
   module = "";
   program = "bundle_derived/bundle_derived.cmx";
   all_modules = "basic/basic.cmx derived/derived.cmx";
   ocamlopt.byte;

   flags = "$flg -I bundle_derived -I p -I p_int -I basic -I derived";
   module = "main_functorize_derived.ml";
   ocamlopt.byte;

   flags = "";
   module = "";
   program = "$test_build_directory/test_functorize_derived.exe";
   all_modules = "\
     basic/basic__.cmx \
     basic/basic.cmx \
     derived/derived.cmx \
     p_int/p_int__.cmx \
     p_int/p_int.cmx \
     bundle_derived/bundle_derived.cmx \
     main_functorize_derived.cmx \
   ";
   ocamlopt.byte;

   stdout = "test_functorize_derived.output";
   stderr = "test_functorize_derived.output";
   output = "test_functorize_derived.output";
   run;

   reference = "test_functorize_derived.reference";
   check-program-output;

   (* Bundle the transparent modules into a second functor *)

   flags = "$flg -functorize -I p -I basic2 -I util2";
   module = "";
   program = "bundle2/bundle2.cmx";
   all_modules = "basic2/basic2.cmx util2/util2.cmx";
   ocamlopt.byte;

   (* Compile and run main using the bundle *)

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

   (* Module type sharing: [Bundle.Make(P_int)()] should match the module
      type [Bundle.Intf(P_int).S].  Same expected output as test_functorize. *)

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

   (* Double application: applying Bundle.Make twice gives fresh abstract types *)

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

   (* Type sharing: with transparent types, R.Basic2.t = R.Util2.t
      after bundle instantiation; two applications give the same type *)

   flags = "$flg -I bundle2 -I p -I p_int -I basic2 -I util2";
   module = "main_functorize_share.ml";
   ocamlopt.byte;

   flags = "";
   module = "";
   program = "$test_build_directory/test_functorize_share.exe";
   all_modules = "\
     basic2/basic2.cmx \
     util2/util2.cmx \
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

   (* -functorize with .cmi inputs: generate bundle CMI only, no CMO *)

   flags = "$flg -functorize -I p -I basic -I util";
   module = "";
   program = "bundle_stopafter/bundle.cmi";
   all_modules = "basic/basic.cmi util/util.cmi";
   ocamlopt.byte;

   script = "sh -c 'test -f bundle_stopafter/bundle.cmi && ! test -f bundle_stopafter/bundle.cmx'";
   script;

   (* Two-phase: phase 1 (.cmi inputs) generates CMI, phase 2 (.cmx inputs) generates CMO *)

   flags = "$flg -functorize -I p -I basic -I util";
   module = "";
   program = "bundle_cmifile/bundle.cmi";
   all_modules = "basic/basic.cmi util/util.cmi";
   ocamlopt.byte;

   flags = "$flg -functorize -I p -I basic -I util";
   module = "";
   program = "bundle_cmifile/bundle.cmx";
   all_modules = "basic/basic.cmx util/util.cmx";
   ocamlopt.byte;

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

   (* CMI generation: .cmi inputs -> .cmi only; verify signature with objinfo *)

   flags = "$flg -functorize -I p -I basic -I util";
   module = "";
   program = "bundle_phases/bundle.cmi";
   all_modules = "basic/basic.cmi util/util.cmi";
   ocamlopt.byte;

   script = "sh -c 'test -f bundle_phases/bundle.cmi && ! test -f bundle_phases/bundle.cmx'";
   script;

   program = "-no-approx -no-code bundle_phases/bundle.cmi";
   output = "bundle_phases.cmi.objinfo.output";
   ocamlobjinfo;

   reference = "bundle_phases.cmi.objinfo.reference";
   check-program-output;

   (* One-step CMO generation: .cmx inputs -> both .cmx and .cmi *)

   flags = "$flg -functorize -I p -I basic -I util";
   module = "";
   program = "bundle_onestep/bundle.cmx";
   all_modules = "basic/basic.cmx util/util.cmx";
   ocamlopt.byte;

   script = "sh -c 'test -f bundle_onestep/bundle.cmx && test -f bundle_onestep/bundle.cmi'";
   script;

   (* -bin-annot-cms: functorize should generate an empty .cms *)

   flags = "$flg -functorize -bin-annot-cms -I p -I basic -I util";
   module = "";
   program = "bundle_cms/bundle.cmx";
   all_modules = "basic/basic.cmx util/util.cmx";
   ocamlopt.byte;

   script = "sh -c 'test -f bundle_cms/bundle.cms'";
   script;

   program = "bundle_cms/bundle.cms";
   output = "bundle_cms.cms.objinfo.output";
   ocamlobjinfo;

   reference = "bundle_cms.cms.objinfo_native.reference";
   check-program-output;

   (* Compile the incompatible declared interface for the bad-cmi-file test *)

   flags = "";
   module = "bundle_bad/bundle_bad.mli";
   ocamlopt.byte;

   (* Phase 1 for swap test: generate CMI with Util first *)
   flags = "$flg -functorize -I p -I basic -I util";
   module = "";
   program = "bundle_swap/bundle_swap.cmi";
   all_modules = "util/util.cmi basic/basic.cmi";
   ocamlopt.byte;

   (* CRC mismatch: phase 1 generates bundle_crc.cmi (Basic+Util), phase 2
      generates bundle_crc.cmx recording that CRC; then bundle_crc.cmi is
      overwritten with a different signature (Basic only).  Linking must fail. *)

   flags = "$flg -functorize -I p -I basic -I util";
   module = "";
   program = "bundle_crc/bundle_crc.cmi";
   all_modules = "basic/basic.cmi util/util.cmi";
   ocamlopt.byte;

   flags = "$flg -functorize -I p -I basic -I util \
     -cmi-file bundle_crc/bundle_crc.cmi";
   module = "";
   program = "bundle_crc/bundle_crc.cmx";
   all_modules = "basic/basic.cmx util/util.cmx";
   ocamlopt.byte;

   (* Overwrite bundle_crc.cmi with a different signature (Basic only) *)
   flags = "$flg -functorize -I p -I basic";
   module = "";
   program = "bundle_crc/bundle_crc.cmi";
   all_modules = "basic/basic.cmi";
   ocamlopt.byte;

   flags = "$flg -I bundle_crc -I p -I p_int -I basic";
   module = "main_crc_mismatch.ml";
   ocamlopt.byte;

   (* Freshness: abstract types from two bundle applications cannot be mixed *)

   {
     flags = "$flg -I bundle -I p -I p_int -I basic -I util";
     module = "bad_mix_bundles.ml";
     ocamlopt_byte_exit_status = "2";
     compiler_output = "bad_mix_bundles.output";
     ocamlopt.byte;

     compiler_reference = "bad_mix_bundles.reference";
     check-ocamlopt.byte-output;
   }

   (* Inclusion error: -cmi-file declares Make as a plain structure,
      but the bundle infers a functor.  The inclusion check must fail. *)
   {
     flags = "$flg -functorize -I p -I basic -I util \
       -cmi-file bundle_bad/bundle_bad.cmi";
     module = "";
     program = "bundle_bad/bundle_bad.cmx";
     all_modules = "basic/basic.cmx util/util.cmx";
     ocamlopt_byte_exit_status = "2";
     compiler_output = "bad_cmi_file.output";
     ocamlopt.byte;

     compiler_reference = "bad_cmi_file.reference";
     check-ocamlopt.byte-output;
   }

   (* Inclusion error: declared CMI has Util before Basic; bundle CMO has
      Basic before Util.  With the [Intf]/[Make] cmi shape, [module type S
      = sig ... end] inside [Intf] is type-level positional, so a runtime
      permutation can't be coerced through it — the inclusion check must
      fail. *)
   {
     flags = "$flg -functorize -I p -I basic -I util \
       -cmi-file bundle_swap/bundle_swap.cmi";
     module = "";
     program = "bundle_swap/bundle_swap.cmx";
     all_modules = "basic/basic.cmx util/util.cmx";
     ocamlopt_byte_exit_status = "2";
     compiler_output = "bad_swap.output";
     ocamlopt.byte;

     compiler_reference = "bad_swap.reference";
     check-ocamlopt.byte-output;
   }

   (* CRC mismatch: bundle_crc.cmx recorded CRC of bundle_crc.cmi v1;
      bundle_crc.cmi was then replaced with v2 (different signature).
      The linker must detect the CRC inconsistency. *)
   {
     flags = "";
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
     compiler_output = "bad_crc_mismatch.output";
     ocamlopt.byte;

     compiler_reference = "bad_crc_mismatch_native.reference";
     check-ocamlopt.byte-output;
   }

 {
   (* Real module alias: functorizer must track it as a concrete dependency
      even when compiled with -no-alias-deps. *)

   flags = "$flg -parameter P -I p -I message";
   module = "message/message.mli message/message.ml";
   ocamlopt.byte;

   flags = "$flg -parameter P -I p -I message";
   module = "with_message/with_message.ml";
   ocamlopt.byte;

   flags = "$flg -functorize -I p -I message -I with_message";
   module = "";
   program = "bundle_msg/bundle_msg.cmx";
   all_modules = "message/message.cmx with_message/with_message.cmx";
   ocamlopt.byte;
 }

 {
   (* Alias-only parameterised module: [pure_alias] declares [-parameter P]
      but its body never mentions P; its body [module Message = Message]
      aliases a sibling parameterised module.  Two-phase functorize must
      agree on the bundle's signature across phases (the original
      [cu_format = [Rp_unit]] bug); path compression in [compute_bundle_sig]
      ensures the alias chain is collapsed so the bundle's signature is
      well-formed when later consumed by a client. *)

   flags = "$flg -parameter P -I p -I pure_alias";
   module = "pure_alias/pure_alias.ml";
   ocamlopt.byte;

   flags = "$flg -functorize -I p -I message -I pure_alias";
   module = "";
   program = "bundle_pure_alias/bundle_pure_alias.cmi";
   all_modules = "pure_alias/pure_alias.cmi";
   ocamlopt.byte;

   flags = "$flg -functorize -I p -I message -I pure_alias \
     -cmi-file bundle_pure_alias/bundle_pure_alias.cmi";
   module = "";
   program = "bundle_pure_alias/bundle_pure_alias.cmx";
   all_modules = "pure_alias/pure_alias.cmx";
   ocamlopt.byte;

   (* Consumer: apply the bundle's functor and access values through both
      the direct path ([Inst.Message]) and the alias chain
      ([Inst.Pure_alias.Message]).  Pre-path-compression, the alias chain
      failed because the saved bundle's signature treated the alias target
      as a global reference. *)

   flags = "$flg -I bundle_pure_alias -I p -I p_int -I message";
   module = "main_pure_alias.ml";
   ocamlopt.byte;

   flags = "";
   module = "";
   program = "$test_build_directory/test_functorize_pure_alias.exe";
   all_modules = "\
     message/message.cmx \
     pure_alias/pure_alias.cmx \
     p_int/p_int__.cmx \
     p_int/p_int.cmx \
     bundle_pure_alias/bundle_pure_alias.cmx \
     main_pure_alias.cmx \
   ";
   ocamlopt.byte;

   stdout = "test_functorize_pure_alias.output";
   stderr = "test_functorize_pure_alias.output";
   output = "test_functorize_pure_alias.output";
   run;

   reference = "test_functorize_pure_alias.reference";
   check-program-output;
 }

 {
   (* Non-parameterised root: [uses_plain] is parameterised by P and
      references [Plain] (a non-parameterised module).  Path compression
      should stop at [Plain] without interning it into the bundle, so the
      bundle's signature is just [functor (P) () -> sig module Uses_plain
      end] with [Plain] left as a global. *)

   flags = "$flg -I plain";
   module = "plain/plain.mli plain/plain.ml";
   ocamlopt.byte;

   flags = "$flg -parameter P -I p -I plain -I uses_plain";
   module = "uses_plain/uses_plain.ml";
   ocamlopt.byte;

   flags = "$flg -functorize -I p -I plain -I uses_plain";
   module = "";
   program = "bundle_uses_plain/bundle_uses_plain.cmx";
   all_modules = "uses_plain/uses_plain.cmx";
   ocamlopt.byte;

   flags = "$flg -I bundle_uses_plain -I p -I p_int -I plain";
   module = "main_uses_plain.ml";
   ocamlopt.byte;

   flags = "";
   module = "";
   program = "$test_build_directory/test_functorize_uses_plain.exe";
   all_modules = "\
     plain/plain.cmx \
     uses_plain/uses_plain.cmx \
     p_int/p_int__.cmx \
     p_int/p_int.cmx \
     bundle_uses_plain/bundle_uses_plain.cmx \
     main_uses_plain.cmx \
   ";
   ocamlopt.byte;

   stdout = "test_functorize_uses_plain.output";
   stderr = "test_functorize_uses_plain.output";
   output = "test_functorize_uses_plain.output";
   run;

   reference = "test_functorize_uses_plain.reference";
   check-program-output;

   (* User error: passing a non-parameterised module as functorize input
      should be rejected with a clear message rather than silently
      producing an empty/useless bundle. *)
   {
     flags = "$flg -functorize -I plain";
     module = "";
     program = "bundle_bad_non_param/bundle_bad_non_param.cmi";
     all_modules = "plain/plain.cmi";
     ocamlopt_byte_exit_status = "2";
     compiler_output = "bad_non_param_input.output";
     ocamlopt.byte;

     compiler_reference = "bad_non_param_input.reference";
     check-ocamlopt.byte-output;
   }
 }

 {
   (* Bundle basic_share and util_share: util_share.t = basic_share.t *)

   flags = "$flg -functorize -I p -I basic_share -I util_share";
   module = "";
   program = "bundle_share/bundle_share.cmx";
   all_modules = "basic_share/basic_share.cmx util_share/util_share.cmx";
   ocamlopt.byte;

   (* Type sharing + fresh counter test *)

   flags = "$flg -I bundle_share -I p -I p_int -I basic_share -I util_share";
   module = "main_functorize_type_share.ml";
   ocamlopt.byte;

   flags = "";
   module = "";
   program = "$test_build_directory/test_functorize_type_share.exe";
   all_modules = "\
     basic_share/basic_share.cmx \
     util_share/util_share.cmx \
     p_int/p_int__.cmx \
     p_int/p_int.cmx \
     bundle_share/bundle_share.cmx \
     main_functorize_type_share.cmx \
   ";
   ocamlopt.byte;

   stdout = "test_functorize_type_share.output";
   stderr = "test_functorize_type_share.output";
   output = "test_functorize_type_share.output";
   run;

   reference = "test_functorize_type_share.reference";
   check-program-output;

   (* Freshness: abstract types from two bundle_share applications cannot be mixed *)

   {
     flags = "$flg -I bundle_share -I p -I p_int -I basic_share -I util_share";
     module = "bad_mix_share.ml";
     ocamlopt_byte_exit_status = "2";
     ocamlopt.byte;
   }
 }}
*)

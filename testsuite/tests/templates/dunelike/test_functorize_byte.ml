(* TEST
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
   main_pure_alias.reference \
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
   bundle_cms.cms.objinfo_byte.reference \
   bundle_bad.mli \
   bad_swap.reference \
   bad_cmi_file.reference \
   main_crc_mismatch.ml \
   bad_crc_mismatch_byte.reference \
 ";
 {
   setup-ocamlc.byte-build-env;

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
   ocamlc.byte;

   flags = "$flg -as-parameter -I p -open P__";
   module = "p/p.mli";
   ocamlc.byte;

   (* Basic, parameterized by P *)

   flags = "$flg_int_iface -parameter P -I p";
   module = "basic/basic__.ml";
   ocamlc.byte;

   flags = "$flg -parameter P -I p -I basic -open Basic__";
   module = "basic/basic.mli basic/basic.ml";
   ocamlc.byte;

   (* Util, parameterized by P *)

   flags = "$flg_int_iface -parameter P -I p";
   module = "util/util__.ml";
   ocamlc.byte;

   flags = "$flg -parameter P -I p -I util -open Util__";
   module = "util/util.mli util/util.ml";
   ocamlc.byte;

   (* P_int, argument for P *)

   flags = "$flg_int_iface";
   module = "p_int/p_int__.ml";
   ocamlc.byte;

   flags = "$flg -as-argument-for P -I p -I p_int -open P_int__";
   module = "p_int/p_int.mli p_int/p_int.ml";
   ocamlc.byte;

   (* Transparent modules: basic2 and util2 expose type t = P.t *)

   flags = "$flg -parameter P -I p";
   module = "basic2/basic2.ml";
   ocamlc.byte;

   flags = "$flg -parameter P -I p";
   module = "util2/util2.ml";
   ocamlc.byte;

   (* Derived: parameterized by P, depends on Basic *)

   src = "derived.mli derived.ml";
   dst = "derived/";
   copy;

   flags = "$flg -parameter P -I p -I basic -I derived";
   module = "derived/derived.mli derived/derived.ml";
   ocamlc.byte;

   (* basic_share: abstract type t, with a counter *)

   flags = "$flg -parameter P -I p -I basic_share";
   module = "basic_share/basic_share.mli basic_share/basic_share.ml";
   ocamlc.byte;

   (* util_share: type t = Basic_share.t, shares type with basic_share *)

   flags = "$flg -parameter P -I p -I basic_share -I util_share";
   module = "util_share/util_share.mli util_share/util_share.ml";
   ocamlc.byte;

   (* Bundle basic and util together into a single functor *)

   flags = "$flg -functorize -I p -I basic -I util";
   module = "";
   program = "bundle/bundle.cmo";
   all_modules = "basic/basic.cmo util/util.cmo";
   ocamlc.byte;

   (* Bundle basic and derived: derived depends on basic *)

   flags = "$flg -functorize -I p -I basic -I derived";
   module = "";
   program = "bundle_derived/bundle_derived.cmo";
   all_modules = "basic/basic.cmo derived/derived.cmo";
   ocamlc.byte;

   flags = "$flg -I bundle_derived -I p -I p_int -I basic -I derived";
   module = "main_functorize_derived.ml";
   ocamlc.byte;

   flags = "";
   module = "";
   program = "$test_build_directory/test_functorize_derived.bc";
   all_modules = "\
     basic/basic__.cmo \
     basic/basic.cmo \
     derived/derived.cmo \
     p_int/p_int__.cmo \
     p_int/p_int.cmo \
     bundle_derived/bundle_derived.cmo \
     main_functorize_derived.cmo \
   ";
   ocamlc.byte;

   stdout = "test_functorize_derived.output";
   stderr = "test_functorize_derived.output";
   output = "test_functorize_derived.output";
   run;

   reference = "test_functorize_derived.reference";
   check-program-output;

   (* Bundle the transparent modules into a second functor *)

   flags = "$flg -functorize -I p -I basic2 -I util2";
   module = "";
   program = "bundle2/bundle2.cmo";
   all_modules = "basic2/basic2.cmo util2/util2.cmo";
   ocamlc.byte;

   (* Compile and run main using the bundle *)

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

   (* Module type sharing: [Bundle.Make(P_int)()] should match the module
      type [Bundle.Intf(P_int).S].  Same expected output as test_functorize. *)

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

   (* Double application: applying Bundle.Make twice gives fresh abstract types *)

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

   (* Type sharing: with transparent types, R.Basic2.t = R.Util2.t
      after bundle instantiation; two applications give the same type *)

   flags = "$flg -I bundle2 -I p -I p_int -I basic2 -I util2";
   module = "main_functorize_share.ml";
   ocamlc.byte;

   flags = "";
   module = "";
   program = "$test_build_directory/test_functorize_share.bc";
   all_modules = "\
     basic2/basic2.cmo \
     util2/util2.cmo \
     p_int/p_int__.cmo \
     p_int/p_int.cmo \
     bundle2/bundle2.cmo \
     main_functorize_share.cmo \
   ";
   ocamlc.byte;

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
   ocamlc.byte;

   script = "sh -c 'test -f bundle_stopafter/bundle.cmi && ! test -f bundle_stopafter/bundle.cmo'";
   script;

   (* Two-phase: phase 1 (.cmi inputs) generates CMI, phase 2 (.cmo inputs) generates CMO *)

   flags = "$flg -functorize -I p -I basic -I util";
   module = "";
   program = "bundle_cmifile/bundle.cmi";
   all_modules = "basic/basic.cmi util/util.cmi";
   ocamlc.byte;

   flags = "$flg -functorize -I p -I basic -I util";
   module = "";
   program = "bundle_cmifile/bundle.cmo";
   all_modules = "basic/basic.cmo util/util.cmo";
   ocamlc.byte;

   flags = "$flg -I bundle_cmifile -I p -I p_int -I basic -I util";
   module = "main_functorize.ml";
   ocamlc.byte;

   flags = "";
   module = "";
   program = "$test_build_directory/test_functorize_cmifile.bc";
   all_modules = "\
     basic/basic__.cmo \
     util/util__.cmo \
     basic/basic.cmo \
     util/util.cmo \
     p_int/p_int__.cmo \
     p_int/p_int.cmo \
     bundle_cmifile/bundle.cmo \
     main_functorize.cmo \
   ";
   ocamlc.byte;

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
   ocamlc.byte;

   script = "sh -c 'test -f bundle_phases/bundle.cmi && ! test -f bundle_phases/bundle.cmo'";
   script;

   program = "-no-approx -no-code bundle_phases/bundle.cmi";
   output = "bundle_phases.cmi.objinfo.output";
   ocamlobjinfo;

   reference = "bundle_phases.cmi.objinfo.reference";
   check-program-output;

   (* One-step CMO generation: .cmo inputs -> both .cmo and .cmi *)

   flags = "$flg -functorize -I p -I basic -I util";
   module = "";
   program = "bundle_onestep/bundle.cmo";
   all_modules = "basic/basic.cmo util/util.cmo";
   ocamlc.byte;

   script = "sh -c 'test -f bundle_onestep/bundle.cmo && test -f bundle_onestep/bundle.cmi'";
   script;

   (* -bin-annot-cms: functorize should generate an empty .cms *)

   flags = "$flg -functorize -bin-annot-cms -I p -I basic -I util";
   module = "";
   program = "bundle_cms/bundle.cmo";
   all_modules = "basic/basic.cmo util/util.cmo";
   ocamlc.byte;

   script = "sh -c 'test -f bundle_cms/bundle.cms'";
   script;

   program = "bundle_cms/bundle.cms";
   output = "bundle_cms.cms.objinfo.output";
   ocamlobjinfo;

   reference = "bundle_cms.cms.objinfo_byte.reference";
   check-program-output;

   (* Compile the incompatible declared interface for the bad-cmi-file test *)

   flags = "";
   module = "bundle_bad/bundle_bad.mli";
   ocamlc.byte;

   (* Phase 1 for swap test: generate CMI with Util first *)
   flags = "$flg -functorize -I p -I basic -I util";
   module = "";
   program = "bundle_swap/bundle_swap.cmi";
   all_modules = "util/util.cmi basic/basic.cmi";
   ocamlc.byte;

   (* CRC mismatch: phase 1 generates bundle_crc.cmi (Basic+Util), phase 2
      generates bundle_crc.cmo recording that CRC; then bundle_crc.cmi is
      overwritten with a different signature (Basic only).  Linking must fail. *)

   flags = "$flg -functorize -I p -I basic -I util";
   module = "";
   program = "bundle_crc/bundle_crc.cmi";
   all_modules = "basic/basic.cmi util/util.cmi";
   ocamlc.byte;

   flags = "$flg -functorize -I p -I basic -I util \
     -cmi-file bundle_crc/bundle_crc.cmi";
   module = "";
   program = "bundle_crc/bundle_crc.cmo";
   all_modules = "basic/basic.cmo util/util.cmo";
   ocamlc.byte;

   (* Overwrite bundle_crc.cmi with a different signature (Basic only) *)
   flags = "$flg -functorize -I p -I basic";
   module = "";
   program = "bundle_crc/bundle_crc.cmi";
   all_modules = "basic/basic.cmi";
   ocamlc.byte;

   flags = "$flg -I bundle_crc -I p -I p_int -I basic";
   module = "main_crc_mismatch.ml";
   ocamlc.byte;

   (* Freshness: abstract types from two bundle applications cannot be mixed *)

   {
     flags = "$flg -I bundle -I p -I p_int -I basic -I util";
     module = "bad_mix_bundles.ml";
     ocamlc_byte_exit_status = "2";
     compiler_output = "bad_mix_bundles.output";
     ocamlc.byte;

     compiler_reference = "bad_mix_bundles.reference";
     check-ocamlc.byte-output;
   }

   (* Inclusion error: -cmi-file declares Make as a plain structure,
      but the bundle infers a functor.  The inclusion check must fail. *)
   {
     flags = "$flg -functorize -I p -I basic -I util \
       -cmi-file bundle_bad/bundle_bad.cmi";
     module = "";
     program = "bundle_bad/bundle_bad.cmo";
     all_modules = "basic/basic.cmo util/util.cmo";
     ocamlc_byte_exit_status = "2";
     compiler_output = "bad_cmi_file.output";
     ocamlc.byte;

     compiler_reference = "bad_cmi_file.reference";
     check-ocamlc.byte-output;
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
     program = "bundle_swap/bundle_swap.cmo";
     all_modules = "basic/basic.cmo util/util.cmo";
     ocamlc_byte_exit_status = "2";
     compiler_output = "bad_swap.output";
     ocamlc.byte;

     compiler_reference = "bad_swap.reference";
     check-ocamlc.byte-output;
   }

   (* CRC mismatch: bundle_crc.cmo recorded CRC of bundle_crc.cmi v1;
      bundle_crc.cmi was then replaced with v2 (different signature).
      The linker must detect the CRC inconsistency. *)
   {
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
     compiler_output = "bad_crc_mismatch.output";
     ocamlc.byte;

     compiler_reference = "bad_crc_mismatch_byte.reference";
     check-ocamlc.byte-output;
   }

 {
   (* Real module alias: functorizer must track it as a concrete dependency
      even when compiled with -no-alias-deps. *)

   flags = "$flg -parameter P -I p -I message";
   module = "message/message.mli message/message.ml";
   ocamlc.byte;

   flags = "$flg -parameter P -I p -I message";
   module = "with_message/with_message.ml";
   ocamlc.byte;

   flags = "$flg -functorize -I p -I message -I with_message";
   module = "";
   program = "bundle_msg/bundle_msg.cmo";
   all_modules = "message/message.cmo with_message/with_message.cmo";
   ocamlc.byte;
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
   ocamlc.byte;

   flags = "$flg -functorize -I p -I message -I pure_alias";
   module = "";
   program = "bundle_pure_alias/bundle_pure_alias.cmi";
   all_modules = "pure_alias/pure_alias.cmi";
   ocamlc.byte;

   flags = "$flg -functorize -I p -I message -I pure_alias \
     -cmi-file bundle_pure_alias/bundle_pure_alias.cmi";
   module = "";
   program = "bundle_pure_alias/bundle_pure_alias.cmo";
   all_modules = "pure_alias/pure_alias.cmo";
   ocamlc.byte;

   (* Consumer: with prune-all-approx, [pure_alias]'s [module Message = Message]
      is an Approximate [cmi_globals] edge, so functorize rewrites it to the
      [Module_alias_pruned] sentinel instead of bundling [Message].  Accessing
      it through the bundle therefore fails to compile: the sentinel module is
      missing.  Pins down that an approximate sibling alias is intentionally not
      carried through functorization. *)
   {
     flags = "$flg -I bundle_pure_alias -I p -I p_int -I message";
     module = "main_pure_alias.ml";
     ocamlc_byte_exit_status = "2";
     compiler_output = "main_pure_alias.output";
     ocamlc.byte;

     compiler_reference = "main_pure_alias.reference";
     check-ocamlc.byte-output;
   }
 }

 {
   (* Non-parameterised root: [uses_plain] is parameterised by P and
      references [Plain] (a non-parameterised module).  Path compression
      should stop at [Plain] without interning it into the bundle, so the
      bundle's signature is just [functor (P) () -> sig module Uses_plain
      end] with [Plain] left as a global. *)

   flags = "$flg -I plain";
   module = "plain/plain.mli plain/plain.ml";
   ocamlc.byte;

   flags = "$flg -parameter P -I p -I plain -I uses_plain";
   module = "uses_plain/uses_plain.ml";
   ocamlc.byte;

   flags = "$flg -functorize -I p -I plain -I uses_plain";
   module = "";
   program = "bundle_uses_plain/bundle_uses_plain.cmo";
   all_modules = "uses_plain/uses_plain.cmo";
   ocamlc.byte;

   flags = "$flg -I bundle_uses_plain -I p -I p_int -I plain";
   module = "main_uses_plain.ml";
   ocamlc.byte;

   flags = "";
   module = "";
   program = "$test_build_directory/test_functorize_uses_plain.bc";
   all_modules = "\
     plain/plain.cmo \
     uses_plain/uses_plain.cmo \
     p_int/p_int__.cmo \
     p_int/p_int.cmo \
     bundle_uses_plain/bundle_uses_plain.cmo \
     main_uses_plain.cmo \
   ";
   ocamlc.byte;

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
     ocamlc_byte_exit_status = "2";
     compiler_output = "bad_non_param_input.output";
     ocamlc.byte;

     compiler_reference = "bad_non_param_input.reference";
     check-ocamlc.byte-output;
   }
 }

 {
   (* Bundle basic_share and util_share: util_share.t = basic_share.t *)

   flags = "$flg -functorize -I p -I basic_share -I util_share";
   module = "";
   program = "bundle_share/bundle_share.cmo";
   all_modules = "basic_share/basic_share.cmo util_share/util_share.cmo";
   ocamlc.byte;

   (* Type sharing + fresh counter test *)

   flags = "$flg -I bundle_share -I p -I p_int -I basic_share -I util_share";
   module = "main_functorize_type_share.ml";
   ocamlc.byte;

   flags = "";
   module = "";
   program = "$test_build_directory/test_functorize_type_share.bc";
   all_modules = "\
     basic_share/basic_share.cmo \
     util_share/util_share.cmo \
     p_int/p_int__.cmo \
     p_int/p_int.cmo \
     bundle_share/bundle_share.cmo \
     main_functorize_type_share.cmo \
   ";
   ocamlc.byte;

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
     ocamlc_byte_exit_status = "2";
     ocamlc.byte;
   }
 }}
*)

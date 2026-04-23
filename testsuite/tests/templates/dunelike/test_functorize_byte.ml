(* TEST
 readonly_files = "\
   p.mli p__.ml \
   basic.mli basic.ml basic__.ml \
   util.mli util.ml util__.ml \
   basic2.ml util2.ml \
   basic_share.mli basic_share.ml \
   util_share.mli util_share.ml \
   p_int.mli p_int.ml p_int__.ml \
   main_functorize.ml \
   main_functorize_double.ml \
   main_functorize_share.ml \
   main_functorize_type_share.ml \
   bad_mix_bundles.ml \
   bad_mix_share.ml \
   test_functorize.reference \
   test_functorize_double.reference \
   test_functorize_share.reference \
   test_functorize_type_share.reference \
   bad_mix_bundles.reference \
 ";
 {
   setup-ocamlc.byte-build-env;

   set OCAMLPARAM = "";

   script = "mkdir p basic util basic2 util2 basic_share util_share p_int bundle bundle2 bundle_share bundle_stopafter bundle_cmifile";
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

   set flg = "-no-alias-deps -w -53";
   set flg_int_iface = "$flg -w -49";

   (* Parameter P *)

   flags = "$flg_int_iface";
   module = "p/p__.ml";
   ocamlc.byte;

   flags = "$flg -as-parameter -I p -open P__ -open No_direct_access_to_p";
   module = "p/p.mli";
   ocamlc.byte;

   (* Basic, parameterized by P *)

   flags = "$flg_int_iface -parameter P -I p";
   module = "basic/basic__.ml";
   ocamlc.byte;

   flags = "\
     $flg -parameter P -I p -I basic \
     -open Basic__ -open No_direct_access_to_basic \
   ";
   module = "basic/basic.mli basic/basic.ml";
   ocamlc.byte;

   (* Util, parameterized by P *)

   flags = "$flg_int_iface -parameter P -I p";
   module = "util/util__.ml";
   ocamlc.byte;

   flags = "\
     $flg -parameter P -I p -I util \
     -open Util__ -open No_direct_access_to_util \
   ";
   module = "util/util.mli util/util.ml";
   ocamlc.byte;

   (* P_int, argument for P *)

   flags = "$flg_int_iface";
   module = "p_int/p_int__.ml";
   ocamlc.byte;

   flags = "\
     $flg -as-argument-for P -I p -I p_int \
     -open P_int__ -open No_direct_access_to_p_int \
   ";
   module = "p_int/p_int.mli p_int/p_int.ml";
   ocamlc.byte;

   (* Transparent modules: basic2 and util2 expose type t = P.t *)

   flags = "$flg -parameter P -I p";
   module = "basic2/basic2.ml";
   ocamlc.byte;

   flags = "$flg -parameter P -I p";
   module = "util2/util2.ml";
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

   flags = "$flg -functorize-intf -I p -I basic -I util";
   module = "";
   program = "bundle/bundle.cmi";
   all_modules = "basic/basic.cmi util/util.cmi";
   ocamlc.byte;

   flags = "$flg -functorize-impl -I p -I basic -I util";
   module = "";
   program = "bundle/bundle.cmo";
   all_modules = "basic/basic.cmo util/util.cmo";
   ocamlc.byte;

   (* Bundle the transparent modules into a second functor *)

   flags = "$flg -functorize-intf -I p -I basic2 -I util2";
   module = "";
   program = "bundle2/bundle2.cmi";
   all_modules = "basic2/basic2.cmi util2/util2.cmi";
   ocamlc.byte;

   flags = "$flg -functorize-impl -I p -I basic2 -I util2";
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

   (* Double application: applying Bundle.Func twice gives fresh abstract types *)

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

   (* -functorize-intf: generate bundle CMI only, no CMO *)

   flags = "$flg -functorize-intf -I p -I basic -I util";
   module = "";
   program = "bundle_stopafter/bundle.cmi";
   all_modules = "basic/basic.cmi util/util.cmi";
   ocamlc.byte;

   script = "sh -c 'test -f bundle_stopafter/bundle.cmi && ! test -f bundle_stopafter/bundle.cmo'";
   script;

   (* Two-phase: phase 1 generates CMI, phase 2 generates CMO *)

   flags = "$flg -functorize-intf -I p -I basic -I util";
   module = "";
   program = "bundle_cmifile/bundle.cmi";
   all_modules = "basic/basic.cmi util/util.cmi";
   ocamlc.byte;

   flags = "$flg -functorize-impl -cmi-file bundle_cmifile/bundle.cmi -I p -I basic -I util";
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

 {
   (* Bundle basic_share and util_share: util_share.t = basic_share.t *)

   flags = "$flg -functorize-intf -I p -I basic_share -I util_share";
   module = "";
   program = "bundle_share/bundle_share.cmi";
   all_modules = "basic_share/basic_share.cmi util_share/util_share.cmi";
   ocamlc.byte;

   flags = "$flg -functorize-impl -I p -I basic_share -I util_share";
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

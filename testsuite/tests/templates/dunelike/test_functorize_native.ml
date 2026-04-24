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
   main_functorize_double.ml \
   main_functorize_derived.ml \
   main_functorize_share.ml \
   main_functorize_type_share.ml \
   bad_mix_bundles.ml \
   bad_mix_share.ml \
   test_functorize.reference \
   test_functorize_double.reference \
   test_functorize_derived.reference \
   test_functorize_share.reference \
   test_functorize_type_share.reference \
   bad_mix_bundles.reference \
 ";
 {
   setup-ocamlopt.byte-build-env;

   set OCAMLPARAM = "";

   script = "mkdir p basic util basic2 util2 basic_share util_share derived p_int bundle bundle2 bundle_derived bundle_share bundle_stopafter bundle_cmifile";
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
   ocamlopt.byte;

   flags = "$flg -as-parameter -I p -open P__ -open No_direct_access_to_p";
   module = "p/p.mli";
   ocamlopt.byte;

   (* Basic, parameterized by P *)

   flags = "$flg_int_iface -parameter P -I p";
   module = "basic/basic__.ml";
   ocamlopt.byte;

   flags = "\
     $flg -parameter P -I p -I basic \
     -open Basic__ -open No_direct_access_to_basic \
   ";
   module = "basic/basic.mli basic/basic.ml";
   ocamlopt.byte;

   (* Util, parameterized by P *)

   flags = "$flg_int_iface -parameter P -I p";
   module = "util/util__.ml";
   ocamlopt.byte;

   flags = "\
     $flg -parameter P -I p -I util \
     -open Util__ -open No_direct_access_to_util \
   ";
   module = "util/util.mli util/util.ml";
   ocamlopt.byte;

   (* P_int, argument for P *)

   flags = "$flg_int_iface";
   module = "p_int/p_int__.ml";
   ocamlopt.byte;

   flags = "\
     $flg -as-argument-for P -I p -I p_int \
     -open P_int__ -open No_direct_access_to_p_int \
   ";
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

   flags = "$flg -functorize-intf -I p -I basic -I util";
   module = "";
   program = "bundle/bundle.cmi";
   all_modules = "basic/basic.cmi util/util.cmi";
   ocamlopt.byte;

   flags = "$flg -functorize-impl -I p -I basic -I util";
   module = "";
   program = "bundle/bundle.cmx";
   all_modules = "basic/basic.cmx util/util.cmx";
   ocamlopt.byte;

   (* Bundle basic and derived: derived depends on basic *)

   flags = "$flg -functorize-intf -I p -I basic -I derived";
   module = "";
   program = "bundle_derived/bundle_derived.cmi";
   all_modules = "basic/basic.cmi derived/derived.cmi";
   ocamlopt.byte;

   flags = "$flg -functorize-impl -I p -I basic -I derived";
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

   flags = "$flg -functorize-intf -I p -I basic2 -I util2";
   module = "";
   program = "bundle2/bundle2.cmi";
   all_modules = "basic2/basic2.cmi util2/util2.cmi";
   ocamlopt.byte;

   flags = "$flg -functorize-impl -I p -I basic2 -I util2";
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

   (* Double application: applying Bundle.Func twice gives fresh abstract types *)

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

   (* -functorize-intf: generate bundle CMI only, no CMO *)

   flags = "$flg -functorize-intf -I p -I basic -I util";
   module = "";
   program = "bundle_stopafter/bundle.cmi";
   all_modules = "basic/basic.cmi util/util.cmi";
   ocamlopt.byte;

   script = "sh -c 'test -f bundle_stopafter/bundle.cmi && ! test -f bundle_stopafter/bundle.cmx'";
   script;

   (* Two-phase: phase 1 generates CMI, phase 2 generates CMO *)

   flags = "$flg -functorize-intf -I p -I basic -I util";
   module = "";
   program = "bundle_cmifile/bundle.cmi";
   all_modules = "basic/basic.cmi util/util.cmi";
   ocamlopt.byte;

   flags = "$flg -functorize-impl -cmi-file bundle_cmifile/bundle.cmi -I p -I basic -I util";
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

 {
   (* Bundle basic_share and util_share: util_share.t = basic_share.t *)

   flags = "$flg -functorize-intf -I p -I basic_share -I util_share";
   module = "";
   program = "bundle_share/bundle_share.cmi";
   all_modules = "basic_share/basic_share.cmi util_share/util_share.cmi";
   ocamlopt.byte;

   flags = "$flg -functorize-impl -I p -I basic_share -I util_share";
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

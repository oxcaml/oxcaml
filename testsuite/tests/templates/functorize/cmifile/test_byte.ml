(* TEST
 (* [-cmi-file] — use a pre-existing [.cmi] instead of generating one.

    1. Two-phase compile (phase 1 emits [.cmi], phase 2 emits [.cmo]
       against it) is runtime-equivalent to the one-step [.cmo] target.
       Also checks which artifacts each phase emits: with [-bin-annot]
       and [-bin-annot-cms], the [.cmi] target adds the interface
       annotations ([.cmti]/[.cmsi]) and the [.cmo] target the
       implementation ones ([.cmt]/[.cms]).
    2. Negative: a [-cmi-file] declaring [Make] as a plain structure
       fails the inclusion check against the inferred functor.
    3. A valid [-cmi-file] declaring a narrower signature than the
       inferred one (only [Make], exposing only [Basic]): the coerced
       module block has fewer fields than the inferred [Intf; Make]
       pair. *)

 readonly_files = "\
   main_functorize.ml test_functorize.reference \
   bundle_bad.mli \
   bad_cmi_file_struct.reference \
   bundle_narrow.mli \
   main_narrow.ml \
   test_narrow.reference \
   bundle.cmi.objinfo.reference \
   bundle.cms.objinfo_byte.reference \
 ";

 setup-ocamlc.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p basic util p_int \
                 bundle_cmifile bundle_bad bundle_narrow";
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

 src = "bundle_narrow.mli";
 dst = "bundle_narrow/";
 copy;

 set flg_base = "-w -53";
 set flg = "$flg_base -no-alias-deps -nocwd";
 set flg_int_iface = "$flg -w -49";

 (* dune does not pass [-nocwd] to link *)
 set flg_link = "$flg_base -no-alias-deps";

 (* Parameter P. *)

 flags = "$flg_int_iface";
 module = "p/p__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -H p -open-cmi p/p__.cmi";
 module = "p/p.mli";
 ocamlc.byte;

 (* [Basic] and [Util]. *)

 flags = "$flg_int_iface -parameter P -I p";
 module = "basic/basic__.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -I p -H basic -open-cmi basic/basic__.cmi";
 module = "basic/basic.mli basic/basic.ml";
 ocamlc.byte;

 flags = "$flg_int_iface -parameter P -I p";
 module = "util/util__.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -I p -H util -open-cmi util/util__.cmi";
 module = "util/util.mli util/util.ml";
 ocamlc.byte;

 (* [P_int] argument for P. *)

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for P -I p -H p_int -open-cmi p_int/p_int__.cmi";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlc.byte;

 {
   (* (1) Positive: phase 1 generates the bundle [.cmi].  With
      [-bin-annot]/[-bin-annot-cms], a [.cmi] target also emits the
      interface annotations ([.cmti]/[.cmsi]) and nothing else. *)

   flags = "$flg -functorize -bin-annot -bin-annot-cms \
     -I p -I basic -I util Basic Util";
   module = "";
   program = "bundle_cmifile/bundle.cmi";
   all_modules = "";
   ocamlc.byte;

   script = "sh -c 'test -f bundle_cmifile/bundle.cmi && \
                    test -f bundle_cmifile/bundle.cmti && \
                    test -f bundle_cmifile/bundle.cmsi && \
                    ! test -f bundle_cmifile/bundle.cmo && \
                    ! test -f bundle_cmifile/bundle.cmt && \
                    ! test -f bundle_cmifile/bundle.cms'";
   script;

   (* Verify the cmi signature via objinfo. *)

   program = "-no-approx -no-code bundle_cmifile/bundle.cmi";
   output = "bundle.cmi.objinfo.output";
   ocamlobjinfo;

   reference = "bundle.cmi.objinfo.reference";
   check-program-output;

   (* Phase 2: generate the [.cmo] against the [.cmi] from phase 1.  With
      [-cmi-file], no [.cmi] is (re)written by this invocation; the
      annotations emitted are the implementation ones ([.cmt]/[.cms]). *)

   flags = "$flg -functorize -bin-annot -bin-annot-cms \
     -I p -I basic -I util \
     -cmi-file bundle_cmifile/bundle.cmi Basic Util";
   module = "";
   program = "bundle_cmifile/bundle.cmo";
   all_modules = "";
   ocamlc.byte;

   script = "sh -c 'test -f bundle_cmifile/bundle.cmo && \
                    test -f bundle_cmifile/bundle.cmt && \
                    test -f bundle_cmifile/bundle.cms'";
   script;

   program = "bundle_cmifile/bundle.cms";
   output = "bundle.cms.objinfo.output";
   ocamlobjinfo;

   reference = "bundle.cms.objinfo_byte.reference";
   check-program-output;

   (* Verify the two-phase bundle is runtime-equivalent to the one-step
      bundle: the consumer program ([main_functorize.ml]) is the one used by
      [basic_util], and the reference is the same [test_functorize.reference]. *)

   flags = "$flg -I bundle_cmifile -I p -I p_int -I basic -I util";
   module = "main_functorize.ml";
   ocamlc.byte;

   flags = "$flg_link";
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
 }{
   (* Compile [bundle_bad.mli] — declares [Make] as a plain structure. *)

   flags = "";
   module = "bundle_bad/bundle_bad.mli";
   ocamlc.byte;

   (* (2) Negative: declared cmi has [Make] as a struct; the bundle infers
      a functor.  Inclusion check rejects. *)

   flags = "$flg -functorize -I p -I basic -I util \
     -cmi-file bundle_bad/bundle_bad.cmi Basic Util";
   module = "";
   program = "bundle_bad/bundle_bad.cmo";
   all_modules = "";
   ocamlc_byte_exit_status = "2";
   compiler_output = "bad_cmi_file_struct.output";
   ocamlc.byte;

   compiler_reference = "bad_cmi_file_struct.reference";
   check-ocamlc.byte-output;
 }{
   (* (3) A valid [-cmi-file] may declare a narrower signature than the
      inferred one (here: only [Make], with its result spelled out and
      exposing only [Basic] of the two bundled modules).  The coerced
      module block then has one field instead of the inferred
      [Intf; Make] pair; the declared main module block format must
      match it. *)

   flags = "";
   module = "bundle_narrow/bundle_narrow.mli";
   ocamlc.byte;

   flags = "$flg -functorize -I p -I basic -I util \
     -cmi-file bundle_narrow/bundle_narrow.cmi Basic Util";
   module = "";
   program = "bundle_narrow/bundle_narrow.cmo";
   all_modules = "";
   ocamlc.byte;

   flags = "$flg -I bundle_narrow -I p -I p_int -I basic";
   module = "main_narrow.ml";
   ocamlc.byte;

   flags = "$flg_link";
   module = "";
   program = "$test_build_directory/test_narrow.bc";
   all_modules = "\
     basic/basic__.cmo \
     util/util__.cmo \
     basic/basic.cmo \
     util/util.cmo \
     p_int/p_int__.cmo \
     p_int/p_int.cmo \
     bundle_narrow/bundle_narrow.cmo \
     main_narrow.cmo \
   ";
   ocamlc.byte;

   stdout = "test_narrow.output";
   stderr = "test_narrow.output";
   output = "test_narrow.output";
   run;

   reference = "test_narrow.reference";
   check-program-output;
 }
*)

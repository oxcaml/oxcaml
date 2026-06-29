(* TEST
 (* Type identity across bundle applications:

    - [Bundle2] (transparent [type t = P.t]): intra-app sharing is
       automatic; cross-app values also mix (both collapse to [int]).
    - [Bundle_share] (abstract [t], sharing via [.mli] equation):
       intra-app [Util_share.t = Basic_share.t] holds; cross-app types
       are fresh so mixing values fails ([bad_mix_share]). *)

 readonly_files = "\
   basic_share.mli basic_share.ml util_share.mli util_share.ml \
   basic2.ml util2.ml \
   main_functorize_share.ml test_functorize_share.reference \
   main_functorize_type_share.ml test_functorize_type_share.reference \
   bad_mix_share.ml \
 ";

 setup-ocamlc.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p p_int basic_share util_share basic2 util2 \
                 bundle_share bundle2";
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

 src = "basic_share.mli basic_share.ml";
 dst = "basic_share/";
 copy;

 src = "util_share.mli util_share.ml";
 dst = "util_share/";
 copy;

 src = "basic2.ml";
 dst = "basic2/";
 copy;

 src = "util2.ml";
 dst = "util2/";
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

 (* [P_int] argument for P. *)

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for P -I p -I p_int -open P_int__";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlc.byte;

 (* [Basic_share] (abstract [t] with counter). *)

 flags = "$flg -parameter P -I p -I basic_share";
 module = "basic_share/basic_share.mli basic_share/basic_share.ml";
 ocamlc.byte;

 (* [Util_share]: [type t = Basic_share.t]. *)

 flags = "$flg -parameter P -I p -I basic_share -I util_share";
 module = "util_share/util_share.mli util_share/util_share.ml";
 ocamlc.byte;

 (* [Basic2] and [Util2]: transparent [type t = P.t]. *)

 flags = "$flg -parameter P -I p";
 module = "basic2/basic2.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -I p";
 module = "util2/util2.ml";
 ocamlc.byte;

 (* Bundle the two flavours. *)

 flags = "$flg -functorize -I p -I basic_share -I util_share \
   Basic_share Util_share";
 module = "";
 program = "bundle_share/bundle_share.cmo";
 all_modules = "";
 ocamlc.byte;

 flags = "$flg -functorize -I p -I basic2 -I util2 Basic2 Util2";
 module = "";
 program = "bundle2/bundle2.cmo";
 all_modules = "";
 ocamlc.byte;

 (* (1) Positive: transparent sharing — within-app + cross-app value mix. *)

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

 (* (2) Positive: abstract+eq sharing — within-app share + counter
    independence across applications. *)

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

 (* (3) Negative: cross-application value mixing fails for the abstract+eq
    bundle.  (The same conclusion holds for plain-abstract bundles, but is
    less interesting since there's no intra-bundle sharing to keep apart
    from cross-application freshness.) *)

 flags = "$flg -I bundle_share -I p -I p_int -I basic_share -I util_share";
 module = "bad_mix_share.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
*)

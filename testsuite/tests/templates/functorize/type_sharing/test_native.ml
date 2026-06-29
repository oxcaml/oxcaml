(* TEST (* DO NOT EDIT. Instead edit type_sharing/test_byte.ml and run gen-native.sh. *)
 (* Type identity around bundle applications.  Two bundle shapes, three
    assertions:

    - [Bundle2]: [Basic2] + [Util2] both expose transparent [type t = P.t]
      (no [.mli]).  Sharing is automatic: after [R = Bundle2.Make(P_int)()],
      [R.Basic2.t = R.Util2.t = P_int.t = int].

    - [Bundle_share]: [Basic_share] (abstract [t] with global counter) +
      [Util_share] (declares [type t = Basic_share.t] in its [.mli]).
      Sharing is established by the [.mli] equation; cross-application
      types are still fresh.

    1. Positive ([main_functorize_share]) — transparent: within an
       application, [Util2.t] flows into [Basic2.t]; across applications,
       [R1.Basic2.t = R2.Basic2.t] (both are [int]) so values mix freely.

    2. Positive ([main_functorize_type_share]) — abstract with equation:
       within an application, [Util_share.t = Basic_share.t]; across two
       applications, each [Basic_share]'s global counter is independent.

    3. Negative ([bad_mix_share]) — abstract with equation: cross-application
       value mixing fails because [R1.Basic_share.t] and [R2.Basic_share.t]
       are fresh abstract types despite the intra-bundle sharing equation. *)

 readonly_files = "\
   basic_share.mli basic_share.ml util_share.mli util_share.ml \
   basic2.ml util2.ml \
   main_functorize_share.ml test_functorize_share.reference \
   main_functorize_type_share.ml test_functorize_type_share.reference \
   bad_mix_share.ml \
 ";

 setup-ocamlopt.byte-build-env;

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
 ocamlopt.byte;

 flags = "$flg -as-parameter -I p -open P__";
 module = "p/p.mli";
 ocamlopt.byte;

 (* [P_int] argument for P. *)

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlopt.byte;

 flags = "$flg -as-argument-for P -I p -I p_int -open P_int__";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlopt.byte;

 (* [Basic_share] (abstract [t] with counter). *)

 flags = "$flg -parameter P -I p -I basic_share";
 module = "basic_share/basic_share.mli basic_share/basic_share.ml";
 ocamlopt.byte;

 (* [Util_share]: [type t = Basic_share.t]. *)

 flags = "$flg -parameter P -I p -I basic_share -I util_share";
 module = "util_share/util_share.mli util_share/util_share.ml";
 ocamlopt.byte;

 (* [Basic2] and [Util2]: transparent [type t = P.t]. *)

 flags = "$flg -parameter P -I p";
 module = "basic2/basic2.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p";
 module = "util2/util2.ml";
 ocamlopt.byte;

 (* Bundle the two flavours. *)

 flags = "$flg -functorize -I p -I basic_share -I util_share \
   Basic_share Util_share";
 module = "";
 program = "bundle_share/bundle_share.cmx";
 all_modules = "";
 ocamlopt.byte;

 flags = "$flg -functorize -I p -I basic2 -I util2 Basic2 Util2";
 module = "";
 program = "bundle2/bundle2.cmx";
 all_modules = "";
 ocamlopt.byte;

 (* (1) Positive: transparent sharing — within-app + cross-app value mix. *)

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

 (* (2) Positive: abstract+eq sharing — within-app share + counter
    independence across applications. *)

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

 (* (3) Negative: cross-application value mixing fails for the abstract+eq
    bundle.  (The same conclusion holds for plain-abstract bundles, but is
    less interesting since there's no intra-bundle sharing to keep apart
    from cross-application freshness.) *)

 flags = "$flg -I bundle_share -I p -I p_int -I basic_share -I util_share";
 module = "bad_mix_share.ml";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
*)

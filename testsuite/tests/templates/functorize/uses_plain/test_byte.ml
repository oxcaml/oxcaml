(* TEST
 (* Non-bundleable dependencies: [uses_plain] is parameterised by P and
    references two dependencies that should NOT be interned into the
    bundle:

    - [Plain] — a non-parameterised compunit (never had any params).
    - [Basic-P_int] — a fully-instantiated instance compunit, produced by
      [ocamlc -instantiate] from [Basic] (parameterised by P) with
      [P:=P_int].

    Neither appears in [uses_plain]'s cmi [bound_globals], so both stay
    as global references in the bundle's signature.  The bundle's signature
    is just [functor (P) () -> sig module Uses_plain end]. *)

 readonly_files = "\
   uses_plain.ml main_uses_plain.ml print_bundle_sig.ml \
   test_functorize_uses_plain.reference \
   print_bundle_sig.inferred.reference \
 ";

 setup-ocamlc.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p p_int basic instances plain uses_plain bundle_uses_plain";
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

 src = "${test_source_directory}/../../dunelike/basic.mli \
        ${test_source_directory}/../../dunelike/basic.ml \
        ${test_source_directory}/../../dunelike/basic__.ml";
 dst = "basic/";
 copy;

 src = "${test_source_directory}/../plain.mli \
        ${test_source_directory}/../plain.ml";
 dst = "plain/";
 copy;

 src = "uses_plain.ml";
 dst = "uses_plain/";
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

 (* [P_int] is an argument for P. *)

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for P -I p -I p_int -open P_int__";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlc.byte;

 (* [Basic] parameterised by P. *)

 flags = "$flg_int_iface -parameter P -I p";
 module = "basic/basic__.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -I p -I basic -open Basic__";
 module = "basic/basic.mli basic/basic.ml";
 ocamlc.byte;

 (* Produce the fully-instantiated [Basic-P_int] via [-instantiate]
    (both the [Basic__] prelude and [Basic] body instances). *)

 flags = "$flg -I p -I p_int -I basic -instantiate";
 module = "";
 program = "instances/basic__-P_int.cmo";
 all_modules = "basic/basic__.cmo p_int/p_int.cmo";
 ocamlc.byte;

 flags = "$flg -I p -I p_int -I basic -instantiate";
 module = "";
 program = "instances/basic-P_int.cmo";
 all_modules = "basic/basic.cmo p_int/p_int.cmo";
 ocamlc.byte;

 (* [Plain] is non-parameterised. *)

 flags = "$flg -I plain";
 module = "plain/plain.mli plain/plain.ml";
 ocamlc.byte;

 (* [Uses_plain] references [Plain] and [Basic-P_int]; neither shows up
    in its cmi [bound_globals]. *)

 flags = "$flg -parameter P -I p -I p_int -I plain -I basic -I instances \
   -I uses_plain";
 module = "uses_plain/uses_plain.ml";
 ocamlc.byte;

 (* Bundle [Uses_plain] only — [Plain] and [Basic-P_int] are left as
    global references. *)

 flags = "$flg -functorize -I p -I plain -I basic -I instances \
   -I uses_plain Uses_plain";
 module = "";
 program = "bundle_uses_plain/bundle_uses_plain.cmo";
 all_modules = "";
 ocamlc.byte;

 (* Consumer. *)

 flags = "$flg -I bundle_uses_plain -I p -I p_int -I plain -I basic \
   -I instances";
 module = "main_uses_plain.ml";
 ocamlc.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize_uses_plain.bc";
 all_modules = "\
   plain/plain.cmo \
   basic/basic__.cmo \
   basic/basic.cmo \
   p_int/p_int__.cmo \
   p_int/p_int.cmo \
   instances/basic__-P_int.cmo \
   instances/basic-P_int.cmo \
   uses_plain/uses_plain.cmo \
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

 (* Verify the bundle's expanded signature: [Bundle_uses_plain.Intf(P_int).S]
    refers to [Plain] and [Basic[P:P_int]] as global paths, not as bundled
    modules. *)

 flags = "$flg -i -I bundle_uses_plain -I p -I p_int -I plain -I basic \
   -I instances";
 module = "print_bundle_sig.ml";
 compiler_output = "print_bundle_sig.inferred.output";
 ocamlc.byte;

 compiler_reference = "print_bundle_sig.inferred.reference";
 check-ocamlc.byte-output;
*)

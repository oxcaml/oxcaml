(* TEST
 (* Module aliases under [-no-alias-deps].  Each case bundles one or two
    modules with [-functorize] and observes how alias targets appear in
    the resulting bundle.

    Pruning rule: a module is pulled into the bundle iff some loaded cmi
    lists it as [Exact] in [bound_globals].  If a module appears only as
    [Approximate] (never [Exact] anywhere), the functorizer substitutes
    it with a [Pruned_<head>] placeholder — the bundle still typechecks
    but any consumer path that reaches the pruned module fails. *)

 readonly_files = "\
   message.mli message.ml with_message.ml \
   pure_alias.ml main_pure_alias.ml bad_pure_alias.reference \
   included_alias.ml main_included_alias.ml \
   test_functorize_included_alias.reference \
   lib__.ml mod_a.ml mod_b.ml main_circular.ml \
   test_functorize_circular.reference \
 ";

 setup-ocamlc.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p p_int message with_message pure_alias included_alias \
                 bundle_msg bundle_pure_alias bundle_included_alias \
                 lib bundle_circular";
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

 src = "message.mli message.ml";
 dst = "message/";
 copy;

 src = "with_message.ml";
 dst = "with_message/";
 copy;

 src = "pure_alias.ml";
 dst = "pure_alias/";
 copy;

 src = "included_alias.ml";
 dst = "included_alias/";
 copy;

 src = "lib__.ml mod_a.ml mod_b.ml";
 dst = "lib/";
 copy;

 set flg = "-no-alias-deps -w -53";
 set flg_int_iface = "$flg -w -49";

 (* Parameter [P] and argument [P_int]. *)

 flags = "$flg_int_iface";
 module = "p/p__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -I p -open P__";
 module = "p/p.mli";
 ocamlc.byte;

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for P -I p -I p_int -open P_int__";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlc.byte;

 (* Common alias target [Message], and the three source modules that
    reference it in different ways. *)

 flags = "$flg -parameter P -I p -I message";
 module = "message/message.mli message/message.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -I p -I message";
 module = "with_message/with_message.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -I p -I pure_alias";
 module = "pure_alias/pure_alias.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -I p -I message -I included_alias";
 module = "included_alias/included_alias.ml";
 ocamlc.byte;

 (* dune-style renaming library [Lib__] and its two wrappers, which
    mutually alias each other through [-open Lib__]. *)

 flags = "$flg_int_iface -parameter P -I p";
 module = "lib/lib__.ml";
 ocamlc.byte;

 set flg_lib = "$flg -parameter P -I p -I lib -open Lib__";

 flags = "$flg_lib";
 module = "lib/mod_a.ml";
 ocamlc.byte;

 flags = "$flg_lib";
 module = "lib/mod_b.ml";
 ocamlc.byte;

 (* Case 1 — [with_message.ml] aliases [Message] AND uses [Message.hello]
    in its body.  The body use forces a CRC, so [with_message.cmi] lists
    [Message] as [Exact] and it is pulled into the bundle. *)

 flags = "$flg -functorize -I p -I message -I with_message With_message";
 module = "";
 program = "bundle_msg/bundle_msg.cmo";
 all_modules = "";
 ocamlc.byte;

 (* Case 2 — [pure_alias.ml] declares only [module Message = Message].
    Under [-no-alias-deps] this records [Message] as [Approximate]
    (no CRC), and since no other loaded cmi lists it as [Exact],
    [Message] is pruned to [Pruned_Message].  Bundling succeeds; a
    consumer that touches [Inst.Pure_alias.Message] fails to compile. *)

 flags = "$flg -functorize -I p -I message -I pure_alias Pure_alias";
 module = "";
 program = "bundle_pure_alias/bundle_pure_alias.cmo";
 all_modules = "";
 ocamlc.byte;

 flags = "$flg -I bundle_pure_alias -I p -I p_int -I message";
 module = "main_pure_alias.ml";
 ocamlc_byte_exit_status = "2";
 compiler_output = "bad_pure_alias.output";
 ocamlc.byte;

 compiler_reference = "bad_pure_alias.reference";
 check-ocamlc.byte-output;

 (* Case 3 — [included_alias.ml] uses [module Message = struct include
    Message end].  The [include] forces a body-level use, so its cmi
    records [Message] as [Exact] (same effect as case 1). *)

 flags = "$flg -functorize -I p -I message -I included_alias \
   Included_alias";
 module = "";
 program = "bundle_included_alias/bundle_included_alias.cmo";
 all_modules = "";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;

 flags = "$flg -I bundle_included_alias -I p -I p_int -I message";
 module = "main_included_alias.ml";
 ocamlc.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize_included_alias.bc";
 all_modules = "\
   message/message.cmo \
   included_alias/included_alias.cmo \
   p_int/p_int__.cmo \
   p_int/p_int.cmo \
   bundle_included_alias/bundle_included_alias.cmo \
   main_included_alias.cmo \
 ";
 ocamlc.byte;

 stdout = "test_functorize_included_alias.output";
 stderr = "test_functorize_included_alias.output";
 output = "test_functorize_included_alias.output";
 run;

 reference = "test_functorize_included_alias.reference";
 check-program-output;

 (* Case 4 — circular aliases through a dune-style renaming module.
    [mod_a] and [mod_b] each body-use [Lib__] (recording it [Exact])
    and mutually alias each other.  Bundling [Mod_a Mod_b] together
    makes both top-level inputs [Exact], so the long chain
    [Inst.Mod_a.Mod_b_alias.Mod_a_alias....] resolves at runtime. *)

 flags = "$flg -functorize -I p -I lib Mod_a Mod_b";
 module = "";
 program = "bundle_circular/bundle_circular.cmo";
 all_modules = "";
 ocamlc.byte;

 flags = "$flg -I bundle_circular -I p -I p_int -I lib";
 module = "main_circular.ml";
 ocamlc.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize_circular.bc";
 all_modules = "\
   lib/lib__.cmo \
   lib/mod_a.cmo \
   lib/mod_b.cmo \
   p_int/p_int__.cmo \
   p_int/p_int.cmo \
   bundle_circular/bundle_circular.cmo \
   main_circular.cmo \
 ";
 ocamlc.byte;

 stdout = "test_functorize_circular.output";
 stderr = "test_functorize_circular.output";
 output = "test_functorize_circular.output";
 run;

 reference = "test_functorize_circular.reference";
 check-program-output;
*)

(* TEST (* DO NOT EDIT. Instead edit module_alias/test_byte.ml and run gen-native.sh. *)
 (* Module aliases under [-no-alias-deps].  Every module referenced from
    a loaded cmi's [bound_globals] is loaded and pulled into the bundle,
    whether the reference was recorded [Exact] (body-level use) or
    [Approximate] (pure alias). *)

 readonly_files = "\
   message.mli message.ml with_message.ml \
   pure_alias.ml main_pure_alias.ml \
   test_functorize_pure_alias.reference \
   included_alias.ml main_included_alias.ml \
   test_functorize_included_alias.reference \
   lib__.ml mod_a.ml mod_b.ml main_circular.ml sig_circular.ml \
   sig_circular.reference test_functorize_circular.reference \
 ";

 setup-ocamlopt.byte-build-env;

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

 set flg_base = "-w -53";
 set flg = "$flg_base -no-alias-deps -nocwd";
 set flg_int_iface = "$flg -w -49";

 (* dune does not pass [-nocwd] to link *)
 set flg_link = "$flg_base -no-alias-deps";

 (* Parameter [P] and argument [P_int]. *)

 flags = "$flg_int_iface";
 module = "p/p__.ml";
 ocamlopt.byte;

 flags = "$flg -as-parameter -H p -open-cmi p/p__.cmi";
 module = "p/p.mli";
 ocamlopt.byte;

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlopt.byte;

 flags = "$flg -as-argument-for P -I p -H p_int -open-cmi p_int/p_int__.cmi";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlopt.byte;

 (* Common alias target [Message], and the three source modules that
    reference it in different ways. *)

 flags = "$flg -parameter P -I p -I message";
 module = "message/message.mli message/message.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -I message";
 module = "with_message/with_message.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -I pure_alias";
 module = "pure_alias/pure_alias.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -I message -I included_alias";
 module = "included_alias/included_alias.ml";
 ocamlopt.byte;

 (* dune-style renaming library [Lib__] and its two wrappers, which
    mutually alias each other through the opened [Lib__] prelude. *)

 flags = "$flg_int_iface -parameter P -I p";
 module = "lib/lib__.ml";
 ocamlopt.byte;

 set flg_lib = "$flg -parameter P -I p -H lib -open-cmi lib/lib__.cmi";

 flags = "$flg_lib";
 module = "lib/mod_a.ml";
 ocamlopt.byte;

 flags = "$flg_lib";
 module = "lib/mod_b.ml";
 ocamlopt.byte;

 {
   (* Case 1 — [with_message.ml] aliases [Message] AND uses [Message.hello]
      in its body.  The body use forces a CRC, so [with_message.cmi] lists
      [Message] as [Exact] and it is pulled into the bundle. *)

   flags = "$flg -functorize -I p -I message -I with_message With_message";
   module = "";
   program = "bundle_msg/bundle_msg.cmx";
   all_modules = "";
   ocamlopt.byte;
 }{
   (* Case 2 — [pure_alias.ml] declares only [module Message = Message].
      Under [-no-alias-deps] this records [Message] as [Approximate]
      (no CRC); the functorizer loads [Message]'s cmi anyway and bundles
      it, so a consumer can use [Inst.Pure_alias.Message]. *)

   flags = "$flg -functorize -I p -I message -I pure_alias Pure_alias";
   module = "";
   program = "bundle_pure_alias/bundle_pure_alias.cmx";
   all_modules = "";
   ocamlopt.byte;

   flags = "$flg -I bundle_pure_alias -I p -I p_int -I message";
   module = "main_pure_alias.ml";
   ocamlopt.byte;

   flags = "$flg_link";
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
 }{
   (* Case 3 — [included_alias.ml] uses [module Message = struct include
      Message end].  The [include] forces a body-level use, so its cmi
      records [Message] as [Exact] (same effect as case 1). *)

   flags = "$flg -functorize -I p -I message -I included_alias \
     Included_alias";
   module = "";
   program = "bundle_included_alias/bundle_included_alias.cmx";
   all_modules = "";
   ocamlopt.byte;

   flags = "$flg -I bundle_included_alias -I p -I p_int -I message";
   module = "main_included_alias.ml";
   ocamlopt.byte;

   flags = "$flg_link";
   module = "";
   program = "$test_build_directory/test_functorize_included_alias.exe";
   all_modules = "\
     message/message.cmx \
     included_alias/included_alias.cmx \
     p_int/p_int__.cmx \
     p_int/p_int.cmx \
     bundle_included_alias/bundle_included_alias.cmx \
     main_included_alias.cmx \
   ";
   ocamlopt.byte;

   stdout = "test_functorize_included_alias.output";
   stderr = "test_functorize_included_alias.output";
   output = "test_functorize_included_alias.output";
   run;

   reference = "test_functorize_included_alias.reference";
   check-program-output;
 }{
   (* Case 4 — circular aliases through a dune-style renaming module.
      [mod_a] and [mod_b] each body-use [Lib__] (recording it [Exact])
      and mutually alias each other.  Bundling [Mod_a Mod_b] together
      makes both top-level inputs [Exact], so the long chain
      [Inst.Mod_a.Mod_b_alias.Mod_a_alias....] resolves at runtime. *)

   flags = "$flg -functorize -I p -I lib Mod_a Mod_b";
   module = "";
   program = "bundle_circular/bundle_circular.cmx";
   all_modules = "";
   ocamlopt.byte;

   (* Print the bundle's inferred module structure so the circular alias
      chain is visible in the test output. *)

   flags = "$flg -I bundle_circular -I p -I p_int -I lib -i";
   module = "sig_circular.ml";
   compiler_output = "sig_circular.output";
   ocamlopt.byte;

   compiler_reference = "sig_circular.reference";
   check-ocamlopt.byte-output;

   flags = "$flg -I bundle_circular -I p -I p_int -I lib";
   module = "main_circular.ml";
   ocamlopt.byte;

   flags = "$flg_link";
   module = "";
   program = "$test_build_directory/test_functorize_circular.exe";
   all_modules = "\
     lib/lib__.cmx \
     lib/mod_a.cmx \
     lib/mod_b.cmx \
     p_int/p_int__.cmx \
     p_int/p_int.cmx \
     bundle_circular/bundle_circular.cmx \
     main_circular.cmx \
   ";
   ocamlopt.byte;

   stdout = "test_functorize_circular.output";
   stderr = "test_functorize_circular.output";
   output = "test_functorize_circular.output";
   run;

   reference = "test_functorize_circular.reference";
   check-program-output;
 }
*)

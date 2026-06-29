(* TEST
 (* Module aliases under [-no-alias-deps].  Behaviour hinges on whether the
    alias target shows up in the source cmi's [bound_globals] with [Exact]
    or [Approximate] precision:

    1. [with_message.ml] aliases [Message] AND uses [Message.hello] in its
       body.  The body use forces a CRC, so [with_message.cmi] records
       [Message] with [Exact] precision.  [-functorize With_message] then
       transitively pulls [Message] into the bundle (no need to name it on
       the command line).

    2. [pure_alias.ml] declares only [module Message = Message] (no use).
       Under [-no-alias-deps], [Message] is recorded with [Approximate]
       precision (no CRC).  The functorizer substitutes such references
       with the placeholder [Pruned_<head>].  Bundling itself
       succeeds, but a consumer that accesses [Inst.Pure_alias.Message]
       fails to compile.

    3. [included_alias.ml] uses [module Message = struct include Message
       end] — a workaround for case (2).  The [include] in the body forces
       a body-level use of [Message] so its cmi records [Message] with
       [Exact] precision; transitively pulled in like case (1). *)

 readonly_files = "\
   message.mli message.ml with_message.ml \
   pure_alias.ml main_pure_alias.ml bad_pure_alias.reference \
   included_alias.ml main_included_alias.ml \
   test_functorize_included_alias.reference \
 ";

 setup-ocamlc.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p p_int message with_message pure_alias included_alias \
                 bundle_msg bundle_pure_alias bundle_included_alias";
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

 (* [Message] parameterised by P. *)

 flags = "$flg -parameter P -I p -I message";
 module = "message/message.mli message/message.ml";
 ocamlc.byte;

 (* [With_message] aliases [Message] and uses [Message.hello]. *)

 flags = "$flg -parameter P -I p -I message";
 module = "with_message/with_message.ml";
 ocamlc.byte;

 (* [Pure_alias] is alias-only. *)

 flags = "$flg -parameter P -I p -I pure_alias";
 module = "pure_alias/pure_alias.ml";
 ocamlc.byte;

 (* [Included_alias] uses [include Message] (forces a body-level use). *)

 flags = "$flg -parameter P -I p -I message -I included_alias";
 module = "included_alias/included_alias.ml";
 ocamlc.byte;

 (* Case 1: bundle only [With_message] — [Message] gets pulled in
    transitively via [with_message.cmi]'s [Exact]-precision dependency. *)

 flags = "$flg -functorize -I p -I message -I with_message With_message";
 module = "";
 program = "bundle_msg/bundle_msg.cmo";
 all_modules = "";
 ocamlc.byte;

 (* Case 2: bundle [Pure_alias] alone — bundling succeeds, but the
    bundle's signature now references [Pruned_<head>]. *)

 flags = "$flg -functorize -I p -I message -I pure_alias Pure_alias";
 module = "";
 program = "bundle_pure_alias/bundle_pure_alias.cmo";
 all_modules = "";
 ocamlc.byte;

 (* Consumer of [Pure_alias] bundle — fails because
    [Inst.Pure_alias.Message] resolves to [Pruned_<head>]. *)

 flags = "$flg -I bundle_pure_alias -I p -I p_int -I message";
 module = "main_pure_alias.ml";
 ocamlc_byte_exit_status = "2";
 compiler_output = "bad_pure_alias.output";
 ocamlc.byte;

 compiler_reference = "bad_pure_alias.reference";
 check-ocamlc.byte-output;

 (* Case 3: bundle [Included_alias] alone — [Message] gets pulled in via
    the include's [Exact]-precision dependency.  Consumer works. *)

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
*)

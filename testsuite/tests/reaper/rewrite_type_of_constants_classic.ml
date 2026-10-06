(* TEST
 flambda2;
 setup-ocamlopt.byte-build-env;
 {
   module = "rewrite_type_of_constants_classic.mli";
   ocamlopt.byte;
 }{
   flags = "-Oclassic -no-flambda2-reaper";
   module = "rewrite_type_of_constants_classic.ml";
   ocamlopt.byte;
 }{
   program = "rewrite_type_of_constants_classic.cmx";
   output = "rewrite_type_of_constants_classic.cmx.ocamlobjinfo.no-reaper.output";
   ocamlobjinfo;
 }{
   script = "sh ${test_source_directory}/check-has-123456.sh rewrite_type_of_constants_classic.cmx.ocamlobjinfo.no-reaper.output";
   script;
 }{
   flags = "-Oclassic -flambda2-reaper -X reaper-oclassic=1";
   module = "rewrite_type_of_constants_classic.ml";
   ocamlopt.byte;
 }{
   program = "rewrite_type_of_constants_classic.cmx";
   output = "rewrite_type_of_constants_classic.cmx.ocamlobjinfo.reaper.output";
   ocamlobjinfo;
 }{
   script = "sh ${test_source_directory}/check-no-123456.sh rewrite_type_of_constants_classic.cmx.ocamlobjinfo.reaper.output";
   script;
 }
*)

(* Classic-mode counterpart of rewrite_type_of_constants.ml.

   In classic mode, the types stored in the cmx come from the value
   approximations computed during closure conversion. Here, [x] is a static
   block whose approximation records the constant [123456]; it is kept
   reachable from the cmx because the (inlinable) code of [f] mentions it.

   The second field of [x] is however dead as only [f] is exported. Thus, we
   [123456] must not appear in the output typing env. *)

let x = 0, 123456

let[@inline never] [@local never] id z = z

let f b = if b then fst (id x) else 0

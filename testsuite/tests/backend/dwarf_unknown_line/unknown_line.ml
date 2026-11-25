(* TEST
 native-compiler;
 readonly_files = "ppx_unknown_line.ml call_site.sh";
 include ocamlcommon;
 setup-ocamlopt.byte-build-env;
 program = "${test_build_directory}/ppx_unknown_line.exe";
 all_modules = "ppx_unknown_line.ml";
 ocamlopt.byte;
 program = "";
 all_modules = "";
 module = "unknown_line.ml";
 flags = "-g -gdwarf-inlined-frames \
          -ppx ${test_build_directory}/ppx_unknown_line.exe";
 ocamlopt.byte;
 output = "unknown_line.output";
 script = "bash ${test_source_directory}/call_site.sh ${ocamlsrcdir}/ocamlopt";
 script;
 reference = "${test_source_directory}/unknown_line.reference";
 check-program-output;
*)

(* [f_unknown] and [f_known] are inlined, so each call becomes a
   DW_TAG_inlined_subroutine with DW_AT_call_line and DW_AT_call_column.
   [ppx_unknown_line] gives the call to [f_unknown] an unknown line
   ([pos_lnum = -1]), leaving only a character offset, which is not a column:
   neither attribute should be emitted. The call to [f_known] is the control. *)

let[@inline always] f_unknown x =
  let y = Sys.opaque_identity (x * 3) in
  y + 1

let[@inline always] f_known x =
  let y = Sys.opaque_identity (x * 5) in
  y + 2

let[@inline never] [@local never] g_unknown x =
  let r = f_unknown (Sys.opaque_identity x) in
  Sys.opaque_identity r

let[@inline never] [@local never] g_known x =
  let r = f_known (Sys.opaque_identity x) in
  Sys.opaque_identity r

let _ = g_unknown 11 + g_known 12

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
 script = "sh ${test_source_directory}/call_site.sh";
 script;
 reference = "${test_source_directory}/unknown_line.reference";
 check-program-output;
*)

(* [f_unknown] and [f_known] are inlined into their callers, so each call
   becomes a DW_TAG_inlined_subroutine whose DW_AT_call_line and
   DW_AT_call_column give the position of the call. [ppx_unknown_line] gives
   the call to [f_unknown] a location with an unknown line ([pos_lnum = -1])
   and only a character offset, as ppx-generated code may have. Without a
   line there is no column either: the character offset is not a column, so
   neither attribute may be emitted. The call to [f_known] is the control and
   keeps both. *)

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

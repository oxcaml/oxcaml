(* TEST
 flambda2;
 setup-ocamlopt.opt-build-env;
 ocamlrunparam = "b=0";

 flags = "-flambda2-reaper -support-lto";
 compile_only = "true";
 ocamlopt.opt;

 program = "lto_sections_and_rebuild.cmx";
 output = "paused.objinfo";
 ocamlobjinfo;
 output = "scripts.output";
 script = "grep -q 'Flambda 2 unit with LTO information' paused.objinfo";
 script;

 script = "grep -a -q LTO_UNUSED_MODULE_EXPORT lto_sections_and_rebuild.o";
 script;

 compile_only = "false";
 flags = "-reaper-solve lto_sections_and_rebuild.cmx";
 last_flags = "-o lto_sections_and_rebuild.ltosol";
 all_modules = "";
 ocamlopt.opt;

 file = "lto_sections_and_rebuild.ltosol";
 file-exists;

 flags = "-reaper-rebuild lto_sections_and_rebuild.cmx lto_sections_and_rebuild.ltosol";
 last_flags = "";
 all_modules = "";
 ocamlopt.opt;

 file = "lto_sections_and_rebuild.reaped.cmx";
 file-exists;

 program = "lto_sections_and_rebuild.reaped.cmx";
 output = "reaped.objinfo";
 ocamlobjinfo;
 output = "scripts.output";
 script = "grep -q 'Flambda 2 unit (with no export information)' reaped.objinfo";
 script;

 exit_status = "1";
 script = "grep -q 'Flambda 2 unit with LTO information' reaped.objinfo";
 script;
 script = "grep -a -q LTO_UNUSED_MODULE_EXPORT lto_sections_and_rebuild.reaped.o";
 script;
 exit_status = "0";

 flags = "";
 compile_only = "false";
 all_modules = "lto_sections_and_rebuild.reaped.cmx";
 program = "${test_build_directory}/lto_sections_and_rebuild.exe";
 ocamlopt.opt;
 run;
*)

(* Check that LTO removes an unused export, then link and run the rebuilt unit. *)

(* CR mvellacott: the following line would cause this test to fail, because we
   don't restore [Translmod.primitive_declarations] on resume. *)

(* external unused_stub : unit -> unit = "caml_lto_test_stub" *)

module M : sig
  val go : int -> int
end = struct
  type t =
    { used : int;
      unused : int
    }

  let[@inline never] make x = { used = x; unused = Sys.opaque_identity (x * 100) }

  let[@inline never] read t = t.used

  let go x = read (make x)
end

let[@inline never] unused_export () = print_endline "LTO_UNUSED_MODULE_EXPORT"

let () = assert (Sys.opaque_identity (M.go 3) = 3)

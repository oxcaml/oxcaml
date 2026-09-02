(* TEST
 flambda2;
 setup-ocamlopt.opt-build-env;
 ocamlrunparam = "b=0";

 flags = "-flambda2-reaper -support-lto";
 compile_only = "true";
 ocamlopt.opt;

 file = "cmr_creation_and_rebuild.cmr";
 file-exists;

 compile_only = "false";
 flags = "-reaper-solve cmr_creation_and_rebuild.cmr";
 last_flags = "-o cmr_creation_and_rebuild.ltosol";
 all_modules = "";
 ocamlopt.opt;

 file = "cmr_creation_and_rebuild.ltosol";
 file-exists;

 flags = "-reaper-rebuild cmr_creation_and_rebuild.cmr cmr_creation_and_rebuild.ltosol";
 last_flags = "";
 all_modules = "";
 ocamlopt.opt;

 file = "cmr_creation_and_rebuild.reaped.cmx";
 file-exists;

 flags = "";
 compile_only = "false";
 program = "${test_build_directory}/cmr_creation_and_rebuild.exe";
 all_modules = "cmr_creation_and_rebuild.reaped.cmx";
 ocamlopt.opt;

 run;
*)

(* End-to-end test of the split Reaper flow on a single module: emit a .cmr
   with -support-lto, solve, rebuild from the .ltosol, then link and run the
   result. (The rebuilt object is not compared against per-unit Reaper output:
   the whole-program solve intentionally treats the module block's exports more
   precisely, so the outputs legitimately differ.) *)

(* CR mvellacott: the following line would cause this test to fail, because we
   don't restore [Translmod.primitive_declarations] on resume. *)

(* external unused_stub : unit -> unit = "caml_cmr_test_stub" *)

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

let () = ignore (Sys.opaque_identity (M.go 3) : int)

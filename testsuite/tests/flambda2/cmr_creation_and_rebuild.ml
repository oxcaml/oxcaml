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
 flags = "-reaper-rebuild cmr_creation_and_rebuild.cmr";
 all_modules = "";
 ocamlopt.opt;

 flags = "-flambda2-reaper";
 compile_only = "true";
 all_modules = "cmr_creation_and_rebuild.ml";
 ocamlopt.opt;

 script = "cmp cmr_creation_and_rebuild.reaped.o cmr_creation_and_rebuild.o";
 script;
*)

(* Pausing and resuming the Reaper via -support-lto and -reaper-rebuild
   should produce the same object file as running the Reaper in a single
   -flambda2-reaper invocation. *)

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

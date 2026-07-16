(* TEST
   flambda2;
   compile_only = "true";
   flags += "-flambda2-reaper -reaper-local-fields -reaper-debug-flags=nostamps";
   setup-ocamlopt.opt-build-env;
   {
     ocamlopt.opt;
     script = "cp support_lto.o support_lto.no_lto.o";
     script;
   }{
     flags += " -support-lto -reaper-debug-flags=confirm-run";
     ocamlopt.opt;
     check-ocamlopt.opt-output;
     script = "cmp support_lto.o support_lto.no_lto.o";
     script;
   }
 *)

(* Check that the reaper is idempotent by comparing the binaries after running it once
   and after running it twice (which -support-lto does). We compare at .o level because
   the second pass can re-mangle some names in the IR.

   We also enable a debug print from the reaper so we can confirm it runs twice. *)

let f x =
  let g y =
    let[@inline never][@local never] h u = u in
    let (a, _) = h (x, y) in
    a
  in
  let () = () in
  g

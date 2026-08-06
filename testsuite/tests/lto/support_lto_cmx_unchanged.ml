(* TEST
 flambda2;
 setup-ocamlopt.opt-build-env;
 compile_only = "true";
 flags = "-no-flambda2-reaper";
 ocamlopt.opt;

 src = "support_lto_cmx_unchanged.cmx";
 dst = "support_lto_cmx_unchanged.normal.cmx";
 copy;

 flags = "-flambda2-reaper -support-lto";
 ocamlopt.opt;

 script = "cmp support_lto_cmx_unchanged.cmx support_lto_cmx_unchanged.normal.cmx";
 script;

 check-ocamlopt.opt-output;
*)

(* Compiling with [-flambda2-reaper -support-lto] only does a partial reaper pass and
   leaves the unit unchanged, so it should produce the same .cmx as a normal pass. *)

let[@inline never] f x =
  let g y = x + y in
  g

let[@inline never] pair a b = a, b

let () =
  let h = f 3 in
  let x, y = pair (h 1) (h 2) in
  assert (x + y = 9)

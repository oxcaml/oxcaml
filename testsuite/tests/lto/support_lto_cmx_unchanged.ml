(* TEST
 flambda2;
 setup-ocamlopt.opt-build-env;
 compile_only = "true";
 flags = "-no-flambda2-reaper";
 ocamlopt.opt;

 program = "support_lto_cmx_unchanged.cmx";
 output = "normal.objinfo";
 ocamlobjinfo;

 flags = "-flambda2-reaper -support-lto";
 ocamlopt.opt;

 program = "support_lto_cmx_unchanged.cmx";
 output = "lto.objinfo";
 ocamlobjinfo;

 stdout = "lto.filtered";
 stderr = "lto.filtered";
 script = "grep -v 'Flambda 2 unit with LTO information' lto.objinfo";
 script;

 stdout = "scripts.output";
 stderr = "scripts.output";
 script = "grep -q 'Flambda 2 unit with LTO information' lto.objinfo";
 script;
 script = "cmp normal.objinfo lto.filtered";
 script;

 check-ocamlopt.opt-output;
*)

(* Compiling with [-flambda2-reaper -support-lto] only does a partial reaper pass
   and leaves the unit unchanged, so it should produce the same .cmx as a normal
   pass apart from the LTO sections. (ocamltest runs ocamlobjinfo with
   -null-crc, so the CRC, which the sections change, is not compared.) *)

let[@inline never] f x =
  let g y = x + y in
  g

let[@inline never] pair a b = a, b

let () =
  let h = f 3 in
  let x, y = pair (h 1) (h 2) in
  assert (x + y = 9)

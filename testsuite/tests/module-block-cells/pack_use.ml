(* TEST
 readonly_files = "pack_a.ml pack_b.ml";
 flambda2;
 setup-ocamlopt.byte-build-env;
 flags = "-for-pack Cellpack";
 module = "pack_a.ml";
 ocamlopt.byte;
 module = "pack_b.ml";
 ocamlopt.byte;
 module = "";
 flags = "-pack";
 program = "cellpack.cmx";
 all_modules = "pack_a.cmx pack_b.cmx";
 ocamlopt.byte;
 flags = "";
 module = "pack_use.ml";
 ocamlopt.byte;
 module = "";
 program = "${test_build_directory}/pack_use.exe";
 all_modules = "cellpack.cmx pack_use.cmx";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* A -pack unit builds its block from its members' blocks, which the packer
   rebuilds from the members' cells; the pack itself is then a cells unit. *)

module type A = sig val x : int val r : int ref end
module type B = sig val f : int -> int val s : string end
module type P = sig module Pack_a : A module Pack_b : B end

let () =
  Printf.printf "Pack_a.x = %d, !Pack_a.r = %d\n"
    Cellpack.Pack_a.x !Cellpack.Pack_a.r;
  Printf.printf "Pack_b.f 1 = %d, Pack_b.s = %s\n"
    (Cellpack.Pack_b.f 1) Cellpack.Pack_b.s;
  let module W = (val (module Cellpack : P)) in
  Printf.printf "whole: Pack_a.x = %d, Pack_b.f 1 = %d, Pack_b.s = %s\n"
    W.Pack_a.x (W.Pack_b.f 1) W.Pack_b.s;
  let module WA = (val (module Cellpack.Pack_a : A)) in
  WA.r := 7;
  Printf.printf "after WA.r := 7: !Pack_a.r = %d, !W.Pack_a.r = %d\n"
    !Cellpack.Pack_a.r !W.Pack_a.r

(* TEST
 readonly_files = "cells_lib.ml";
 flambda2;
 {
   (* The library is compiled -opaque: the client has no .cmx information
      about it, so every field is a runtime load from a cell. *)
   setup-ocamlopt.byte-build-env;
   flags = "-opaque";
   module = "cells_lib.ml";
   ocamlopt.byte;
   flags = "";
   module = "opaque_client.ml";
   ocamlopt.byte;
   module = "";
   all_modules = "cells_lib.cmx opaque_client.cmx";
   program = "${test_build_directory}/opaque_client.exe";
   ocamlopt.byte;
   run;
   check-program-output;
 }{
   (* The client is compiled -opaque. *)
   compiler_directory_suffix = ".client";
   setup-ocamlopt.byte-build-env;
   module = "cells_lib.ml";
   ocamlopt.byte;
   flags = "-opaque";
   module = "opaque_client.ml";
   ocamlopt.byte;
   flags = "";
   module = "";
   all_modules = "cells_lib.cmx opaque_client.cmx";
   program = "${test_build_directory}/opaque_client.exe";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Field accesses and a whole-block use of a cells unit without cross-module
   information. *)

external box_float : float# -> float = "%box_float"

module type S = sig
  val f : int -> int
  val n : int
  val r : int ref
  val u : float#
end

let () =
  Printf.printf "fields: f 3 = %d, n = %d, !r = %d, u = %.1f\n"
    (Cells_lib.f 3) Cells_lib.n !Cells_lib.r (box_float Cells_lib.u);
  let module W = (val (module Cells_lib : S)) in
  Printf.printf "whole: f 3 = %d, n = %d, !r = %d, u = %.1f\n"
    (W.f 3) W.n !W.r (box_float W.u);
  W.r := 5;
  Printf.printf "after W.r := 5: !Cells_lib.r = %d\n" !Cells_lib.r

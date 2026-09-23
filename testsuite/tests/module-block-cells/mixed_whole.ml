(* TEST
 readonly_files = "mixed_whole_lib.ml";
 flambda2;
 setup-ocamlopt.byte-build-env;
 module = "mixed_whole_lib.ml";
 ocamlopt.byte;
 module = "mixed_whole.ml";
 ocamlopt.byte;
 module = "";
 all_modules = "mixed_whole_lib.cmx mixed_whole.cmx";
 program = "${test_build_directory}/mixed_whole.exe";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* A unit with unboxed fields, used whole (rebuilt as a mixed block from its
   cells, whose flat fields come after the values) and field-wise. *)

external box_float : float# -> float = "%box_float"

module type S = sig
  val x : float#
  val y : int
  val w : float#
  val p : #(float# * int)
  val f : int -> float
end

let packed = (module Mixed_whole_lib : S)
module M : S = Mixed_whole_lib
module F (X : S) = struct let g () = X.f X.y end
module FM = F (Mixed_whole_lib)

let show name (module X : S) =
  let #(p0, p1) = X.p in
  Printf.printf "%s: x = %.1f, y = %d, w = %.1f, p = (%.1f, %d), f 2 = %.1f\n"
    name (box_float X.x) X.y (box_float X.w) (box_float p0) p1 (X.f 2)

let () =
  show "packed" packed;
  show "M" (module M);
  Printf.printf "FM.g () = %.1f\n" (FM.g ());
  let #(p0, p1) = Mixed_whole_lib.p in
  Printf.printf
    "fields: x = %.1f, y = %d, w = %.1f, p = (%.1f, %d), f 2 = %.1f\n"
    (box_float Mixed_whole_lib.x) Mixed_whole_lib.y
    (box_float Mixed_whole_lib.w) (box_float p0) p1 (Mixed_whole_lib.f 2)

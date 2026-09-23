(* TEST
 readonly_files = "whole_block_lib.ml";
 flambda2;
 setup-ocamlopt.byte-build-env;
 module = "whole_block_lib.ml";
 ocamlopt.byte;
 module = "whole_block.ml";
 ocamlopt.byte;
 module = "";
 all_modules = "whole_block_lib.cmx whole_block.cmx";
 program = "${test_build_directory}/whole_block.exe";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* Whole-block uses of [Whole_block_lib], whose module block native code no
   longer emits: each one is rebuilt from the unit's cells.  [S] lists the
   unit's runtime items in order, so none of these involves a coercion. *)

module type S = sig
  val f : int -> int
  val c : string
  val n : int
  val r : int ref
end

let packed = (module Whole_block_lib : S)
module M : S = Whole_block_lib
module F (X : S) = struct let g () = X.f X.n end
module FM = F (Whole_block_lib)
module I = struct include Whole_block_lib end

(* [S2] reorders and drops items, so this is a coerced pack, as before. *)
module type S2 = sig val n : int val f : int -> int end
let coerced = (module Whole_block_lib : S2)

let show name (module X : S) =
  Printf.printf "%s: f 2 = %d, c = %s, n = %d, !r = %d\n"
    name (X.f 2) X.c X.n !X.r

let () =
  show "packed" packed;
  show "M" (module M);
  show "I" (module I);
  Printf.printf "FM.g () = %d\n" (FM.g ());
  let module C = (val coerced) in
  Printf.printf "coerced: f 2 = %d, n = %d\n" (C.f 2) C.n;
  (* The rebuilt blocks hold the unit's fields, not copies of them. *)
  Whole_block_lib.r := 100;
  let module P = (val packed) in
  Printf.printf "after r := 100: !P.r = %d, !M.r = %d, !I.r = %d\n"
    !P.r !M.r !I.r

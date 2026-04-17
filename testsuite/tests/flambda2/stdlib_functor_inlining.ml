(* TEST
 compile_only = "true";
 flags = "-w +a-70 -warn-error +55";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
*)

(* This test checks that the functors exposed in the stdlib interfaces of
   Hashtbl, Map and Set can all be inlined at -O3.  Warning 55 ("inlining
   impossible") would cause a test failure if any of them could not be
   inlined at the [@inlined] functor application below. *)

module H_key = struct
  type t = int
  let equal = Int.equal
  let hash = Hashtbl.hash
end

module H_seeded_key = struct
  type t = int
  let equal = Int.equal
  let seeded_hash = Hashtbl.seeded_hash
end

module Ord_key = struct
  type t = int
  let compare = Int.compare
end

module H1 = (Hashtbl.Make [@inlined]) (H_key)
module H2 = (Hashtbl.MakePortable [@inlined]) (H_key)
module H3 = (Hashtbl.MakeSeeded [@inlined]) (H_seeded_key)
module H4 = (Hashtbl.MakeSeededPortable [@inlined]) (H_seeded_key)

module M1 = (Map.Make [@inlined]) (Ord_key)
module M2 = (Map.MakePortable [@inlined]) (Ord_key)

module S1 = (Set.Make [@inlined]) (Ord_key)
module S2 = (Set.MakePortable [@inlined]) (Ord_key)

(* Use something from each resulting module to prevent them being discarded
   entirely. *)
let _ = H1.create 0
let _ = H2.create 0
let _ = H3.create 0
let _ = H4.create 0
let _ = M1.empty
let _ = M2.empty
let _ = S1.empty
let _ = S2.empty

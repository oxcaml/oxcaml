(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Formalism validation: a Begin_region and ALL of its End_regions live or die
   atomically (multi-End case via two exclave_ exits).
   Rules: INV.Simplify.RegionPairAtomic (ch. 13), S.Rewrite.Let.DeadRegion (ch. 10),
   INV.Simplify.EffectfulDeletionInventory (ch. 13).
   Case study: middle_end/flambda2/docs/formalism/14-validation/region_pair_atomic.md
   Phenomenon: f's region (local t dead) has Begin + both Ends deleted; g's region
   (local t used by [use]) keeps Begin + both Ends. Never a mixed outcome. *)

external opaque : 'a -> 'a = "%opaque"
external ( + ) : int -> int -> int = "%addint"

type box = { mutable v : int }

let[@inline never] use (local_ t) = t.v

(* dead local alloc, exclave on both branches: Begin + both Ends deleted *)
let[@inline never] f b x =
  let t = local_ { v = x } in
  let _ = t in
  if b then exclave_ { v = x + 1 } else exclave_ { v = x + 2 }

(* live local alloc, exclave on both branches: Begin + both Ends kept *)
let[@inline never] g b x =
  let t = local_ { v = x } in
  let y = use t in
  if b then exclave_ { v = y + 1 } else exclave_ { v = y + 2 }

let r1 = use (f (opaque true) (opaque 5))
let r2 = use (g (opaque true) (opaque 5))

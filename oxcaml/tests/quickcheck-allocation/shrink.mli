(* Type-preserving shrinker. *)

(* [candidates sample] returns smaller well-typed variants of [sample]. The
   driver re-runs the oracle on each and keeps one only if it stays in the same
   quadrant, iterating to a fixpoint. *)
val candidates : Gen.Sample.t -> Gen.Sample.t list

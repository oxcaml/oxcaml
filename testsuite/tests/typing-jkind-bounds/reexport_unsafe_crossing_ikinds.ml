(* TEST flags = "-extension layouts_alpha"; expect;
*)

(* REGRESSION PIN for the W-1 verdict flip (STAGE5F-NOTES.md, "W-1 crossing_read
   concrete-bound double-count").

   [to_unsafe_mode_crossing]'s ikind-derived [crossing_read] must yield the
   BASE-ONLY floor (with-bounds EXCLUDED, since they travel separately in
   [unsafe_with_bounds]). An earlier version lowered the WHOLE jkind, folding
   CONCRETE with-bounds (e.g. [int ref], which resolve to name-free constants)
   into the crossing floor -- double-counting them. That inflated M1.t's
   unsafe_mod_bounds enough that the [equal_unsafe_mode_crossing] guardrail
   (types.ml) wrongly accepted the re-export below, which the pre-ikind
   compiler REJECTS. This pins the guardrail: M1.t and M2.t have DIFFERENT
   unsafe mode crossing, so the re-export must be REJECTED. A future
   crossing_read that folds concrete bounds back in would make this compile,
   re-flipping the verdict. *)

module M1 = struct
  type t : value mod everything with int ref = A of string
  [@@unsafe_allow_any_mode_crossing]
end
[%%expect {|
module M1 :
  sig
    type t : mutable_data = A of string
    [@@unsafe_allow_any_mode_crossing]
  end
|}]

module M2 = struct
  type t : mutable_data with int ref = M1.t = A of string
  [@@unsafe_allow_any_mode_crossing]
end
[%%expect {|
Lines 2-3, characters 2-36:
2 | ..type t : mutable_data with int ref = M1.t = A of string
3 |   [@@unsafe_allow_any_mode_crossing]
Error: This variant or record definition does not match that of type "M1.t"
       They have different unsafe mode crossing behavior:
       Both specify [@@unsafe_allow_any_mode_crossing], but their bounds are not equal
         the original has: mod global forkable unyielding many stateless
         portable aliased immutable contended with int ref
         but this has: mod forkable unyielding many stateless portable with
         int ref
|}]

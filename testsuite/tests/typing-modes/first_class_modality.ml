(* TEST
 flags = "-extension mode_alpha";
 expect;
*)

(* First-class modalities: [(t @@ m)] as a type.

   This file covers syntax, printing, unification (invariant), signature
   inclusion (which accepts a type crossing at least as much as expected),
   mode crossing, and the validation inherited from the other [@@] positions.

   Because a top-level wrapper on an ACTUAL type unpacks automatically, a test
   that wants to observe the wrapper itself must keep it out of top-level
   actual position -- either under a type constructor, or behind the
   explicit-source form of [:>]. Each such case says so. The
   unpacking rule itself is tested in [first_class_modality_unpack.ml]. *)

type t
[%%expect{|
type t
|}]

(* The type round-trips through parsing and printing. *)
type u = (int @@ portable)
[%%expect{|
type u = (int @@ portable)
|}]

(* An identity modality is erased during translation, so [(int @@ nonportable)]
   denotes just [int]. The redundancy warning is inherited from the shared
   modality translation, as at every other [@@] position. *)
type v = (int @@ nonportable)
[%%expect{|
Line 1, characters 17-28:
1 | type v = (int @@ nonportable)
                     ^^^^^^^^^^^
Warning 220 [redundant-modality]: This modality is redundant.

type v = int
|}]

(* The result type is the UNPACKED one: [@@ m] is a boundary annotation, not a
   sticky property, so inference does not re-assert it. *)
let f (x : (int @@ portable)) = x
[%%expect{|
val f : (int @@ portable) -> int = <fun>
|}]

(* Mode crossing: the wrapper crosses according to its modality, so a
   [nonportable] one is usable at [portable]. *)
let cross (x : (t @@ portable) @ nonportable) = (x : _ @ portable)
[%%expect{|
val cross : (t @@ portable) -> t = <fun>
|}]

(* Unification is invariant in the modality: differing bounds do not unify.
   Tested under [list], because at top level the wrapper would unpack on both
   sides and the two [t]s would then unify happily -- which says nothing about
   invariance. *)
let mismatch (x : (t @@ portable) list) = (x : (t @@ global) list)
[%%expect{|
Line 1, characters 43-44:
1 | let mismatch (x : (t @@ portable) list) = (x : (t @@ global) list)
                                               ^
Error: The value "x" has type "(t @@ portable) list"
       but an expression was expected of type "(t @@ global) list"
       Type "(t @@ portable)" is not compatible with type "(t @@ global)"
|}]

(* Signature inclusion, unlike unification, accepts an implementation that
   crosses at least as much as the signature demands. Return position, so
   covariant. *)
module A : sig val f : unit -> (int @@ portable) end = struct
  let f () : (int @@ portable global) = assert false
end
[%%expect{|
module A : sig val f : unit -> (int @@ portable) end @@ stateless
|}]

(* ... and rejects one that crosses less. *)
module B : sig val f : unit -> (int @@ portable global) end = struct
  let f () : (int @@ portable) = assert false
end
[%%expect{|
Lines 1-3, characters 62-3:
1 | ..............................................................struct
2 |   let f () : (int @@ portable) = assert false
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : unit -> (int @@ portable) @@ stateless end
       is not included in
         sig val f : unit -> (int @@ global portable) end
       Values do not match:
         val f : unit -> (int @@ portable) @@ stateless
       is not included in
         val f : unit -> (int @@ global portable)
       The type "unit -> (int @@ portable)" is not compatible with the type
         "unit -> (int @@ global portable)"
       Type "(int @@ portable)" is not compatible with type
         "(int @@ global portable)"
|}]

(* The inclusion ordering must follow variance. In a contravariant position
   (a function parameter) it flips: an implementation may demand LESS crossing
   than the signature promises, but not more. Getting this backwards is
   unsound -- the body would assume a crossing the caller never guaranteed. *)
module C : sig val g : (t @@ portable) -> unit end = struct
  let g (_ : (t @@ portable global)) = ()
end
[%%expect{|
Lines 1-3, characters 53-3:
1 | .....................................................struct
2 |   let g (_ : (t @@ portable global)) = ()
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val g : (t @@ global portable) -> unit @@ stateless end
       is not included in
         sig val g : (t @@ portable) -> unit end
       Values do not match:
         val g : (t @@ global portable) -> unit @@ stateless
       is not included in
         val g : (t @@ portable) -> unit
       The type "(t @@ global portable) -> unit"
       is not compatible with the type "(t @@ portable) -> unit"
       Type "(t @@ global portable)" is not compatible with type
         "(t @@ portable)"
|}]

module D : sig val g : (t @@ portable global) -> unit end = struct
  let g (_ : (t @@ portable)) = ()
end
[%%expect{|
module D : sig val g : (t @@ global portable) -> unit end @@ stateless
|}]

(* The same defect, shown with a consequence: if the contravariant direction
   were reversed, [call] could hand a merely-portable value to a body that
   treats it as [global], escaping the region. Must be rejected at the module
   boundary. *)
module E : sig
  val f : (t @@ portable) @ local -> unit
end = struct
  let f (x : (t @@ global portable) @ local) = ignore (x : _ @ global)
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : (t @@ global portable) @ local) = ignore (x : _ @ global)
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : (t @@ global portable) @ local -> unit @@ portable end
       is not included in
         sig val f : (t @@ portable) @ local -> unit end
       Values do not match:
         val f : (t @@ global portable) @ local -> unit @@ portable
       is not included in
         val f : (t @@ portable) @ local -> unit
       The type "(t @@ global portable) @ local -> unit"
       is not compatible with the type "(t @@ portable) @ local -> unit"
       Type "(t @@ global portable)" is not compatible with type
         "(t @@ portable)"
|}]

(* Coercion [:>] may weaken a modality. Unlike unification (invariant) and
   like inclusion, it accepts a source that crosses at least as much as the
   target. [subtype_rec] handles contravariance by swapping its arguments, so
   the flip in argument position is automatic.

   These use the EXPLICIT-SOURCE form. With an implicit source the coercion
   sees the type of an expression, and a top-level wrapper on an expression
   now unpacks, so [(x :> ...)] would be coercing a bare [t] and would tell us
   nothing about the modality. The explicit source is an expected-type
   position, which packs, so the wrapper survives to be coerced. *)
let weaken (x : (t @@ portable global)) =
  (x : (t @@ portable global) :> (t @@ portable))
[%%expect{|
val weaken : (t @@ global portable) -> (t @@ portable) = <fun>
|}]

let strengthen (x : (t @@ portable)) =
  (x : (t @@ portable) :> (t @@ portable global))
[%%expect{|
Line 2, characters 2-49:
2 |   (x : (t @@ portable) :> (t @@ portable global))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(t @@ portable)" is not a subtype of "(t @@ global portable)"
|}]

(* Forgetting a modality is safe: it only discards the ability to cross. *)
let forget (x : (t @@ portable)) = (x :> t)
[%%expect{|
val forget : (t @@ portable) -> t = <fun>
|}]

(* Inventing one is not: it would claim a crossing the value does not have. *)
let invent (x : t) = (x :> (t @@ portable))
[%%expect{|
Line 1, characters 21-43:
1 | let invent (x : t) = (x :> (t @@ portable))
                         ^^^^^^^^^^^^^^^^^^^^^^
Error: Type "t" is not a subtype of "(t @@ portable)"
|}]

(* Contravariant position: the direction flips. *)
let contra_bad (g : (t @@ portable global) -> unit) =
  (g :> (t @@ portable) -> unit)
[%%expect{|
Line 2, characters 2-32:
2 |   (g :> (t @@ portable) -> unit)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(t @@ global portable) -> unit" is not a subtype of
         "(t @@ portable) -> unit"
       Type "(t @@ portable)" is not a subtype of "(t @@ global portable)"
|}]

let contra_ok (g : (t @@ portable) -> unit) =
  (g :> (t @@ portable global) -> unit)
[%%expect{|
val contra_ok : ((t @@ portable) -> unit) -> (t @@ global portable) -> unit =
  <fun>
|}]

(* Covariant position nested in a type constructor. *)
let nested_ok (x : (t @@ portable global) list) = (x :> (t @@ portable) list)
[%%expect{|
val nested_ok : (t @@ global portable) list -> (t @@ portable) list = <fun>
|}]

(* Contravariance at depth 2: parity must alternate. Depth 1 alone cannot
   distinguish "swaps correctly" from "flips exactly once". *)
let depth2_ok (g : ((t @@ portable) -> unit) -> unit) =
  (g :> (t -> unit) -> unit)
[%%expect{|
val depth2_ok : (((t @@ portable) -> unit) -> unit) -> (t -> unit) -> unit =
  <fun>
|}]

let depth2_bad (g : (t -> unit) -> unit) =
  (g :> ((t @@ portable) -> unit) -> unit)
[%%expect{|
Line 2, characters 2-42:
2 |   (g :> ((t @@ portable) -> unit) -> unit)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(t -> unit) -> unit" is not a subtype of
         "((t @@ portable) -> unit) -> unit"
       Type "t" is not a subtype of "(t @@ portable)"
|}]

(* Objects and rows are the traditional home of [:>]. Forgetting under a
   method type is fine; inventing one is not. *)
let obj_forget (x : < m : (t @@ portable) >) = (x :> < m : t >)
[%%expect{|
val obj_forget : < m : (t @@ portable) > -> < m : t > = <fun>
|}]

let obj_invent (x : < m : t >) = (x :> < m : (t @@ portable) >)
[%%expect{|
Line 1, characters 33-63:
1 | let obj_invent (x : < m : t >) = (x :> < m : (t @@ portable) >)
                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "< m : t >" is not a subtype of "< m : (t @@ portable) >"
       Type "t" is not a subtype of "(t @@ portable)"
|}]

(* Arrow modes are checked against the EXPECTED domain, so the target's
   modality can supply the crossing that lets a [local] argument reach a
   [global]-requiring function. Sound -- a caller really does supply a
   [(t @@ global)] -- and this is the sharpest behaviour the forgetting arm
   enables. The second case shows the modality is what makes the difference. *)
let arrow_mode (g : t @ global -> unit) = (g :> (t @@ global) @ local -> unit)
[%%expect{|
val arrow_mode : (t -> unit) -> (t @@ global) @ local -> unit = <fun>
|}]

let arrow_mode_no_modality (g : t @ global -> unit) = (g :> t @ local -> unit)
[%%expect{|
Line 1, characters 54-78:
1 | let arrow_mode_no_modality (g : t @ global -> unit) = (g :> t @ local -> unit)
                                                          ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "t -> unit" is not a subtype of "t @ local -> unit"
|}]

(* Nested wrappers: this coercion is sound (the source's effective crossing is
   [meet (portable, global)], below the target's) but is rejected, because the
   comparison looks only at the outer bounds. Pinned so that a future
   normalisation change is forced to revisit it. *)
let nested_incomplete (x : ((t @@ portable) @@ global)) =
  (x : ((t @@ portable) @@ global) :> (t @@ portable))
[%%expect{|
Line 2, characters 2-54:
2 |   (x : ((t @@ portable) @@ global) :> (t @@ portable))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "((t @@ portable) @@ global)" is not a subtype of "(t @@ portable)"
|}]

(* Arm ordering against the abbreviation-expanding arms above. *)
type abbrev = (t @@ portable)
[%%expect{|
type abbrev = (t @@ portable)
|}]

let via_abbrev (x : (t @@ portable global)) =
  (x : (t @@ portable global) :> abbrev)
[%%expect{|
val via_abbrev : (t @@ global portable) -> abbrev = <fun>
|}]

let via_abbrev_bad (x : t) = (x :> abbrev)
[%%expect{|
Line 1, characters 29-42:
1 | let via_abbrev_bad (x : t) = (x :> abbrev)
                                 ^^^^^^^^^^^^^
Error: Type "t" is not a subtype of "abbrev" = "(t @@ portable)"
|}]

(* Mode-level sentinels: forgetting a modality must not let the payload be
   used at a mode the modality never licensed. These are the cross-axis
   checks -- the wrapper crosses one axis, the use demands another. *)
let no_launder_global (x : (t @@ portable) @ local) = ((x :> t) : t @ global)
[%%expect{|
Line 1, characters 56-57:
1 | let no_launder_global (x : (t @@ portable) @ local) = ((x :> t) : t @ global)
                                                            ^
Error: This value is "local" to the parent region
         because it is the payload of a first-class modality
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

let no_launder_portable (x : (t @@ global) @ nonportable) =
  ((x :> t) : t @ portable)
[%%expect{|
Line 2, characters 4-5:
2 |   ((x :> t) : t @ portable)
        ^
Error: This value is "nonportable"
         because it is the payload of a first-class modality
         which is "nonportable".
       However, the highlighted expression is expected to be "portable".
|}]

(* ... whereas matching the axis is accepted, so the sentinels above are
   discriminating rather than vacuous. *)
let launder_ok_global (x : (t @@ global) @ local) = ((x :> t) : t @ global)
[%%expect{|
val launder_ok_global : (t @@ global) @ local -> t = <fun>
|}]

(* Coercion with an implicit source must be ground -- a pre-existing OCaml
   restriction, not specific to modalities. The wrapper is inside a tuple so
   that the implicit source still has one to forget: at top level it would
   unpack before [:>] saw it, and the pair below would stop discriminating.
   A ground source is fine... *)
let implicit_ground (x : ((t @@ portable) * t)) = (x :> (t * t))
[%%expect{|
val implicit_ground : (t @@ portable) * t -> t * t = <fun>
|}]

(* ... but a non-ground one needs the explicit-source form. *)
let implicit_nonground (x : ((t @@ portable) * 'a)) = (x :> (t * 'a))
[%%expect{|
Line 1, characters 55-56:
1 | let implicit_nonground (x : ((t @@ portable) * 'a)) = (x :> (t * 'a))
                                                           ^
Error: This expression cannot be coerced to type ""t * 'a""; it has type
         "(t @@ portable) * 'a"
       but is here used with type "t * 'a"
       Type "(t @@ portable)" is not compatible with type "t"
|}]

let explicit_source (x : ('a list @@ portable)) =
  (x : ('a list @@ portable) :> 'a list)
[%%expect{|
val explicit_source : ('a list @@ portable) -> 'a list = <fun>
|}]

(* Private abbreviations. These also pin the arm ordering: the new arms sit
   BELOW the abbreviation-expanding arms, and the error must blame the private
   target rather than the payload. *)
type priv = private (t @@ portable)
[%%expect{|
type priv = private (t @@ portable)
|}]

let priv_forget (x : priv) = (x :> t)
[%%expect{|
val priv_forget : priv -> t = <fun>
|}]

let priv_target (x : (t @@ portable global)) =
  (x : (t @@ portable global) :> priv)
[%%expect{|
Line 2, characters 2-38:
2 |   (x : (t @@ portable global) :> priv)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(t @@ global portable)" is not a subtype of "priv"
|}]

(* Polymorphic variants: forgetting under a payload, and row widening. *)
let variant_forget (x : [ `A of (t @@ portable) ]) = (x :> [ `A of t ])
[%%expect{|
val variant_forget : [ `A of (t @@ portable) ] -> [ `A of t ] = <fun>
|}]

let variant_invent (x : [ `A of t ]) = (x :> [ `A of (t @@ portable) ])
[%%expect{|
Line 1, characters 39-71:
1 | let variant_invent (x : [ `A of t ]) = (x :> [ `A of (t @@ portable) ])
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "[ `A of t ]" is not a subtype of "[ `A of (t @@ portable) ]"
       Type "t" is not a subtype of "(t @@ portable)"
|}]

(* The codomain mirror of the arrow-mode case above: the return-mode check
   consults the EXPECTED codomain, which no longer carries the crossing the
   source had, so this sound coercion is rejected. Conservative, and pinned. *)
let codomain_mirror (g : unit -> (t @@ global) @ local) = (g :> unit -> t @ global)
[%%expect{|
Line 1, characters 58-83:
1 | let codomain_mirror (g : unit -> (t @@ global) @ local) = (g :> unit -> t @ global)
                                                              ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "unit -> (t @@ global) @ local" is not a subtype of "unit -> t"
|}]

(* Validation is shared with the other [@@] positions. *)
type bad = (t @@ global unique)
[%%expect{|
Line 1, characters 17-30:
1 | type bad = (t @@ global unique)
                     ^^^^^^^^^^^^^
Error: The modality "global" can't be used together with "unique"
|}]

(* Ownership regression: the existing [@@] positions must keep their current
   meaning. Only the parenthesised form is a type-level modality. *)
module type S = sig
  val old_ : t @@ portable
  val new_ : (t @@ portable)
end
[%%expect{|
module type S = sig val old_ : t @@ portable val new_ : (t @@ portable) end
|}]

type r = { old_field : t @@ portable; new_field : (t @@ portable) }
[%%expect{|
type r = { old_field : t @@ portable; new_field : (t @@ portable); }
|}]

(* Toplevel value printing goes through [genprintval], which looks through the
   wrapper. *)
let x : (int @@ portable) = 5
[%%expect{|
val x : (int @@ portable) @@ stateless = 5
|}]

(* Nested wrappers are distinct types and do not normalise:
   [((t @@ m1) @@ m2)] is not identified with [(t @@ m1 m2)]. *)
type nested = ((int @@ portable) @@ contended)
[%%expect{|
type nested = ((int @@ portable) @@ contended)
|}]

(* Automatic introduction: a bare value acquires the modality, being checked
   at the modality applied to the expected mode. Both directions of the rule
   are exercised in
   [first_class_modality_unpack.ml]. *)
let auto_intro : (int @@ portable) = 1
[%%expect{|
val auto_intro : (int @@ portable) @@ stateless = 1
|}]

(* TEST
 flags = "-extension mode_alpha";
 expect;
*)

(* Automatic unpacking of first-class modalities.

   A [(t @@ m)] never survives at the top level of an actual or an expected
   type: the modality leaves the type and enters the mode. The reference
   behaviour is the one-field unboxed record [{ x : t @@ m } [@@unboxed]], and
   the cases below are checked against it. Both directions apply the same map
   to the mode, but that map moves comonadic and monadic axes in opposite
   directions, so each axis group is exercised separately. *)

type t
let use_portable (_ : t @ portable) = ()
let use_nonportable (_ : t @ nonportable) = ()
let use_uncontended (_ : t @ uncontended) = ()
let use_contended (_ : t @ contended) = ()
[%%expect{|
type t
val use_portable : t @ portable -> unit = <fun>
val use_nonportable : t -> unit = <fun>
val use_uncontended : t -> unit = <fun>
val use_contended : t @ contended -> unit = <fun>
|}]

(* ACTUAL SIDE, comonadic. A value whose type carries [@@ portable] is a [t]
   at [portable], whatever mode the wrapper itself is at. *)
let elim_comonadic (e : (t @@ portable) @ nonportable) = use_portable e
[%%expect{|
val elim_comonadic : (t @@ portable) -> unit = <fun>
|}]

(* ACTUAL SIDE, monadic. The movement is the other way: unpacking a
   [@@ contended] wrapper produces a CONTENDED value even from an uncontended
   wrapper, exactly as projecting the field of the unboxed record does. So the
   payload is usable at [contended] ... *)
let elim_monadic (e : (t @@ contended) @ uncontended) = use_contended e
[%%expect{|
val elim_monadic : (t @@ contended) -> unit = <fun>
|}]

(* ... and NOT at [uncontended]. Getting this backwards would let the wrapper
   launder a contended value. *)
let elim_monadic_bad (e : (t @@ contended) @ uncontended) = use_uncontended e
[%%expect{|
Line 1, characters 76-77:
1 | let elim_monadic_bad (e : (t @@ contended) @ uncontended) = use_uncontended e
                                                                                ^
Error: This value is "contended"
         because it is the payload of a first-class modality (with some modality).
       However, the highlighted expression is expected to be "uncontended".
|}]

(* Inference does not re-assert the modality: with no annotation on the
   result, the unpacked type is what propagates. *)
let no_resticking (x : (t @@ portable)) = x
[%%expect{|
val no_resticking : (t @@ portable) -> t = <fun>
|}]

(* EXPECTED SIDE. The round trip through annotations: the modality is put back
   by the return annotation, and the body is checked at the modality applied
   to the expected mode. *)
let round_trip (x : (t @@ portable)) : (t @@ portable) = x
[%%expect{|
val round_trip : (t @@ portable) -> (t @@ portable) = <fun>
|}]

(* Introduction from a plain value. This is the case the pre-Stage-4 test file
   pinned as an error under the name [no_auto_intro]. *)
let intro : (int @@ portable) = 1
[%%expect{|
val intro : (int @@ portable) @@ stateless = 1
|}]

(* EXPECTED SIDE, comonadic: the payload must satisfy the modality. *)
let intro_comonadic (y : t @ portable) : (t @@ portable) = y
[%%expect{|
val intro_comonadic : t @ portable -> (t @@ portable) = <fun>
|}]

let intro_comonadic_bad (y : t @ nonportable) : (t @@ portable) = y
[%%expect{|
Line 1, characters 66-67:
1 | let intro_comonadic_bad (y : t @ nonportable) : (t @@ portable) = y
                                                                      ^
Error: This value is "nonportable"
       but is expected to be "portable"
         because it is the payload of a first-class modality (with some modality).
|}]

(* EXPECTED SIDE, monadic: the expected mode is RELAXED rather than tightened,
   so a contended payload may be packed into a wrapper claimed uncontended.
   That is sound only because reading it back re-contends it -- see
   [elim_monadic_bad] above. *)
let intro_monadic (y : t @ contended) : (t @@ contended) = y
[%%expect{|
val intro_monadic : t @ contended -> (t @@ contended) = <fun>
|}]

(* The two halves compose to the identity, not to a laundering device. *)
let no_launder_monadic (y : t @ contended) : t @ uncontended =
  let w : (t @@ contended) = y in
  w
[%%expect{|
Line 3, characters 2-3:
3 |   w
      ^
Error: This value is "contended"
         because it is the payload of a first-class modality (with some modality).
       However, the highlighted expression is expected to be "uncontended".
|}]

(* Same round trip at a mode the modality does license. *)
let launder_ok_monadic (y : t @ contended) : t @ contended =
  let w : (t @@ contended) = y in
  w
[%%expect{|
val launder_ok_monadic : t @ contended -> t @ contended = <fun>
|}]

(* An expression annotation is an EXPECTED-type position, so [(e : (t @@ m))]
   PACKS, exactly like a return-type annotation. The two spellings must agree;
   if they ever diverge again, these two lines are what catches it. *)
let via_constraint (x : (t @@ portable)) = (x : (t @@ portable))
[%%expect{|
val via_constraint : (t @@ portable) -> (t @@ portable) = <fun>
|}]

let via_return (x : (t @@ portable)) : (t @@ portable) = x
[%%expect{|
val via_return : (t @@ portable) -> (t @@ portable) = <fun>
|}]

(* Packing checks the payload at the modality applied to the expected mode, so
   a payload that does not satisfy the modality is rejected. *)
let annot_pack_bad (y : t @ nonportable) = (y : (t @@ portable))
[%%expect{|
Line 1, characters 44-45:
1 | let annot_pack_bad (y : t @ nonportable) = (y : (t @@ portable))
                                                ^
Error: This value is "nonportable"
       but is expected to be "portable"
         because it is the payload of a first-class modality (with some modality).
|}]

(* Because the annotation packs, it is NOT a way to spell elimination: in a
   context wanting a bare [t] it is a type mismatch, not a silent unpack. *)
let annot_is_not_elim (y : t @ portable) : t @ nonportable =
  (y : (t @@ portable))
[%%expect{|
Line 2, characters 2-23:
2 |   (y : (t @@ portable))
      ^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "(t @@ portable)"
       but an expression was expected of type "t"
|}]

(* Explicit elimination needs no form of its own -- [(e : t)] works, because
   the actual side has already unpacked [e]. So both directions stay
   writable. *)
let elim_via_annot (x : (t @@ portable) @ nonportable) = (x : t @ portable)
[%%expect{|
val elim_via_annot : (t @@ portable) -> t = <fun>
|}]

(* Cross-axis sentinels: unpacking must not hand out a mode on an axis the
   modality says nothing about. *)
let cross_axis_bad (e : (t @@ portable) @ local) = (e : t @ global)
[%%expect{|
Line 1, characters 52-53:
1 | let cross_axis_bad (e : (t @@ portable) @ local) = (e : t @ global)
                                                        ^
Error: This value is "local" to the parent region
         because it is the payload of a first-class modality
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

let cross_axis_ok (e : (t @@ global) @ local) = (e : t @ global)
[%%expect{|
val cross_axis_ok : (t @@ global) @ local -> t = <fun>
|}]

(* Linearity (comonadic) and uniqueness (monadic) behave like portability and
   contention respectively. *)
let linearity_bad (y : t @ once) : (t @@ many) = y
[%%expect{|
Line 1, characters 49-50:
1 | let linearity_bad (y : t @ once) : (t @@ many) = y
                                                     ^
Error: This value is "once"
       but is expected to be "many"
         because it is the payload of a first-class modality (with some modality).
|}]

let uniqueness_bad (e : (t @@ aliased)) = (e : t @ unique)
[%%expect{|
Line 1, characters 43-44:
1 | let uniqueness_bad (e : (t @@ aliased)) = (e : t @ unique)
                                               ^
Error: This value is "aliased"
         because it is the payload of a first-class modality (with some modality).
       However, the highlighted expression is expected to be "unique".
|}]

(* MIDDLE modalities. [shareable] and [shared] sit strictly between the
   endpoints of their axes, and the unpacking must carry the exact bound
   rather than rounding it to an endpoint. These cases cannot be checked
   against the unboxed-record encoding, whose kind inference treats a middle
   modality as identity-like; they are checked against [(t @@ m)] directly.

   Comonadic middle, actual side: unpacking gives exactly [shareable] ... *)
let middle_comonadic (x : (t @@ shareable) @ nonportable) = (x : _ @ shareable)
[%%expect{|
val middle_comonadic : (t @@ shareable) -> t = <fun>
|}]

(* ... and not [portable], which would be rounding the bound down. *)
let middle_comonadic_exact (x : (t @@ shareable) @ nonportable) =
  (x : _ @ portable)
[%%expect{|
Line 2, characters 3-4:
2 |   (x : _ @ portable)
       ^
Error: This value is "shareable"
         because it is the payload of a first-class modality
         which is "shareable" because it crosses with something
         which is "nonportable".
       However, the highlighted expression is expected to be "portable".
|}]

(* Comonadic middle, expected side: the payload must reach [shareable], and
   a value that already exceeds it is fine. *)
let middle_comonadic_intro (y : t @ shareable) : (t @@ shareable) = y
[%%expect{|
val middle_comonadic_intro : t @ shareable -> (t @@ shareable) = <fun>
|}]

let middle_comonadic_intro_bad (y : t @ nonportable) : (t @@ shareable) = y
[%%expect{|
Line 1, characters 74-75:
1 | let middle_comonadic_intro_bad (y : t @ nonportable) : (t @@ shareable) = y
                                                                              ^
Error: This value is "nonportable"
       but is expected to be "shareable"
         because it is the payload of a first-class modality (with some modality).
|}]

(* Monadic middle, actual side: unpacking a [@@ shared] wrapper held at
   [uncontended] gives exactly [shared] -- not [uncontended] (which would drop
   the modality) and not [contended] (which would round it up). *)
let middle_monadic (e : (t @@ shared) @ uncontended) = (e : _ @ shared)
[%%expect{|
val middle_monadic : (t @@ shared) -> t @ shared = <fun>
|}]

let middle_monadic_exact (e : (t @@ shared) @ uncontended) =
  (e : _ @ uncontended)
[%%expect{|
Line 2, characters 3-4:
2 |   (e : _ @ uncontended)
       ^
Error: This value is "shared"
         because it is the payload of a first-class modality (with some modality).
       However, the highlighted expression is expected to be "uncontended".
|}]

(* Monadic middle, expected side. Packing a value that is WORSE than the
   modality does not fail outright: the wrapper's own mode rises to absorb the
   difference, which is visible in the inferred return mode. *)
let middle_monadic_intro (y : t @ contended) : (t @@ shared) = y
[%%expect{|
val middle_monadic_intro : t @ contended -> (t @@ shared) @ corrupted = <fun>
|}]

(* ... and that absorption is not a laundering route, because the use site
   unpacks at the raised wrapper mode, not at the modality's constant. *)
let middle_monadic_follow (y : t @ contended) =
  (middle_monadic_intro y : t @ shared)
[%%expect{|
Line 2, characters 3-25:
2 |   (middle_monadic_intro y : t @ shared)
       ^^^^^^^^^^^^^^^^^^^^^^
Error: This value is weaker than "corrupted"
         because it is the payload of a first-class modality
         which is "corrupted"
         because it is a first-class modality wrapping (with some modality) the expression at line 1, characters 63-64.
       However, the highlighted expression is expected to be "shared" or "uncontended".
|}]

let middle_monadic_launder (y : t @ contended) : t @ shared =
  let w : (t @@ shared) = y in
  w
[%%expect{|
Line 3, characters 2-3:
3 |   w
      ^
Error: This value is "corrupted"
         because it is the payload of a first-class modality
         which is "corrupted"
         because it is a first-class modality wrapping (with some modality) the expression at line 2, characters 26-27.
       However, the highlighted expression is expected to be "shared" or "uncontended".
|}]

let middle_monadic_launder_ok (y : t @ shared) : t @ shared =
  let w : (t @@ shared) = y in
  w
[%%expect{|
val middle_monadic_launder_ok : t @ shared -> t @ shared = <fun>
|}]

(* Application results and record projections unpack too, not just
   identifiers. *)
let via_application (f : unit -> (t @@ portable) @ nonportable) =
  use_portable (f ())
[%%expect{|
val via_application : (unit -> (t @@ portable)) -> unit = <fun>
|}]

type r = { fld : (t @@ portable) }
let via_projection (x : r @ nonportable) = use_portable x.fld
[%%expect{|
type r = { fld : (t @@ portable); }
val via_projection : r -> unit = <fun>
|}]

(* Only the top level unpacks. [(t @@ m) list] keeps its modality, because
   there is no mode for it to move into. *)
let nested_keeps (x : (t @@ portable) list) = x
[%%expect{|
val nested_keeps : (t @@ portable) list -> (t @@ portable) list = <fun>
|}]

(* Nested wrappers unpack all the way down in one step. *)
let doubly_nested (e : ((t @@ portable) @@ contended) @ uncontended) =
  use_contended e
[%%expect{|
val doubly_nested : ((t @@ portable) @@ contended) -> unit = <fun>
|}]

(* Inline lambdas passed to a POLYMORPHIC higher-order function. The argument
   is typed against the function's scheme, with the instance reconciled
   afterwards, so what the lambda's own annotations say matters. Annotating
   the parameter is what gives the body something to unpack; with no parameter
   annotation the parameter's type is a fresh variable, nothing unpacks, and a
   return annotation then demands a modality the parameter never had. *)
let hof : ('a -> 'b) -> 'a -> 'b = fun f x -> f x
[%%expect{|
val hof : ('a -> 'b) -> 'a -> 'b = <fun>
|}]

let hof_both_annotations (x : (t @@ portable)) =
  hof (fun (s : (t @@ portable)) : (t @@ portable) -> s) x
[%%expect{|
val hof_both_annotations : (t @@ portable) -> t = <fun>
|}]

let hof_param_annotation (x : (t @@ portable)) =
  hof (fun (s : (t @@ portable)) -> s) x
[%%expect{|
val hof_param_annotation : (t @@ portable) -> t = <fun>
|}]

let hof_return_annotation_only (x : (t @@ portable)) =
  hof (fun s : (t @@ portable) -> s) x
[%%expect{|
Line 2, characters 34-35:
2 |   hof (fun s : (t @@ portable) -> s) x
                                      ^
Error: This value is "nonportable"
       but is expected to be "portable"
         because it is the payload of a first-class modality (with some modality).
|}]

(* Signatures are unaffected: a [val] keeps the wrapper at the top of its
   TYPE, and inclusion still compares wrapper to wrapper. *)
let use_portable_int (_ : int @ portable) = ()
module M : sig val x : (int @@ portable) end = struct
  let x : (int @@ portable) = 1
end
[%%expect{|
val use_portable_int : int @ portable -> unit = <fun>
module M : sig val x : (int @@ portable) end @@ stateless
|}]

(* ... and the value read out of the module unpacks at the use site. *)
let from_module () = use_portable_int M.x
[%%expect{|
val from_module : unit -> unit = <fun>
|}]

(* Patterns need nothing of their own. A variable bound at type [(t @@ m)]
   keeps the wrapper, which is exactly what makes its use sites unpack; and a
   destructuring pattern is matched against the type of the scrutinee, which
   the expression rule has already unpacked. *)
let pattern_keeps_wrapper (x : (t @@ portable)) =
  let y = x in
  use_portable y
[%%expect{|
val pattern_keeps_wrapper : (t @@ portable) -> unit = <fun>
|}]

let pattern_destructure (x : ((t * t) @@ portable)) =
  let (a, _) = x in
  a
[%%expect{|
val pattern_destructure : (t * t @@ portable) -> t = <fun>
|}]

(* The one pattern position that is NOT reached by the expression rule is a
   [Tmod] written directly on a destructuring parameter: there the pattern,
   not an expression, meets the wrapper. Pinned as a known gap. *)
let pattern_param ((a, _) : ((t * t) @@ portable)) = a
[%%expect{|
Line 1, characters 19-25:
1 | let pattern_param ((a, _) : ((t * t) @@ portable)) = a
                       ^^^^^^
Error: This pattern matches values of type "'a * 'b"
       but a pattern was expected which matches values of type
         "(t * t @@ portable)"
|}]

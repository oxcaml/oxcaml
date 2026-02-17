(* TEST
    flags += "-extension mode_alpha";
    expect;
*)

(* This file tests the behavior of redundant or ambiguous modalities.

   Warning 213 is triggered when multiple modalities on the same axis are
   specified, with the later one overriding the earlier one.
*)

(**************************************************************************)
(* Part 1: Two modalities on the same axis (Warning 213) *)
(**************************************************************************)

(* Two modalities on the portability axis *)
module type S = sig
  val x : int @@ portable nonportable
end
[%%expect{|
Line 2, characters 17-25:
2 |   val x : int @@ portable nonportable
                     ^^^^^^^^
Warning 213: This portability is overriden by nonportable later.

Line 2, characters 26-37:
2 |   val x : int @@ portable nonportable
                              ^^^^^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

module type S = sig val x : int end
|}]

(* Two modalities on the contention axis *)
module type S = sig
  val x : int @@ contended uncontended
end
[%%expect{|
Line 2, characters 17-26:
2 |   val x : int @@ contended uncontended
                     ^^^^^^^^^
Warning 213: This contention is overriden by uncontended later.

Line 2, characters 27-38:
2 |   val x : int @@ contended uncontended
                               ^^^^^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

module type S = sig val x : int end
|}]

(* Three modalities on the same axis. *)
module type S = sig
  val x : int @@ portable nonportable shareable
end
[%%expect{|
Line 2, characters 17-25:
2 |   val x : int @@ portable nonportable shareable
                     ^^^^^^^^
Warning 213: This portability is overriden by nonportable later.

Line 2, characters 26-37:
2 |   val x : int @@ portable nonportable shareable
                              ^^^^^^^^^^^
Warning 213: This portability is overriden by shareable later.

module type S = sig val x : int @@ shareable end
|}]

(* Multiple axes with conflicts on each *)
module type S = sig
  val x : int @@ portable nonportable contended uncontended
end
[%%expect{|
Line 2, characters 17-25:
2 |   val x : int @@ portable nonportable contended uncontended
                     ^^^^^^^^
Warning 213: This portability is overriden by nonportable later.

Line 2, characters 38-47:
2 |   val x : int @@ portable nonportable contended uncontended
                                          ^^^^^^^^^
Warning 213: This contention is overriden by uncontended later.

Line 2, characters 26-37:
2 |   val x : int @@ portable nonportable contended uncontended
                              ^^^^^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

Line 2, characters 48-59:
2 |   val x : int @@ portable nonportable contended uncontended
                                                    ^^^^^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

module type S = sig val x : int end
|}]

(**************************************************************************)
(* Part 2: Identity modalities undoing a default non-identity modality *)
(**************************************************************************)

(* Default stateless, val stateful undoes it *)
module type S = sig @@ stateless
  val x : int @@ stateful
end
[%%expect{|
module type S = sig val x : int end
|}]

(* Default unyielding, val yielding undoes it *)
module type S = sig @@ unyielding
  val x : int @@ yielding
end
[%%expect{|
module type S = sig val x : int end
|}]

(* Multiple axes *)
module type S = sig @@ stateless unyielding
  val x : int @@ stateful yielding
end
[%%expect{|
module type S = sig val x : int end
|}]

(**************************************************************************)
(* Part 3: Non-identity modality redundant with default *)
(**************************************************************************)

(* In sig @@ portable, writing portable again is redundant *)
module type S = sig @@ portable
  val x : int @@ portable
end
[%%expect{|
Line 2, characters 17-25:
2 |   val x : int @@ portable
                     ^^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

module type S = sig val x : int @@ portable end
|}]

(* In sig @@ contended, writing contended again is redundant *)
module type S = sig @@ contended
  val x : int @@ contended
end
[%%expect{|
Line 2, characters 17-26:
2 |   val x : int @@ contended
                     ^^^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

module type S = sig val x : int @@ contended end
|}]

(* Multiple axes *)
module type S = sig @@ portable contended
  val x : int @@ portable contended
end
[%%expect{|
Line 2, characters 17-25:
2 |   val x : int @@ portable contended
                     ^^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

Line 2, characters 26-35:
2 |   val x : int @@ portable contended
                              ^^^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

module type S = sig val x : int @@ portable contended end
|}]

(**************************************************************************)
(* Part 4: Explicit legacy modalities that match implicit defaults *)
(**************************************************************************)

(* stateful matches the default for statefulness on val items *)
module type S = sig
  val x : int @@ stateful
end
[%%expect{|
Line 2, characters 17-25:
2 |   val x : int @@ stateful
                     ^^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

module type S = sig val x : int end
|}]

(* yielding matches the default for yieldability on val items *)
module type S = sig
  val x : int @@ yielding
end
[%%expect{|
Line 2, characters 17-25:
2 |   val x : int @@ yielding
                     ^^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

module type S = sig val x : int end
|}]

(* Multiple axes *)
module type S = sig
  val x : int @@ stateful yielding
end
[%%expect{|
Line 2, characters 26-34:
2 |   val x : int @@ stateful yielding
                              ^^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

Line 2, characters 17-25:
2 |   val x : int @@ stateful yielding
                     ^^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

module type S = sig val x : int end
|}]

(**************************************************************************)
(* Part 5: Record fields and constructor arguments with modalities *)
(**************************************************************************)

(* Record field with two modalities on same axis *)
type t = { x : int @@ portable nonportable }
[%%expect{|
Line 1, characters 22-30:
1 | type t = { x : int @@ portable nonportable }
                          ^^^^^^^^
Warning 213: This portability is overriden by nonportable later.

Line 1, characters 31-42:
1 | type t = { x : int @@ portable nonportable }
                                   ^^^^^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

type t = { x : int; }
|}]

(* Constructor argument with two modalities on same axis *)
type t = Foo of int @@ portable nonportable
[%%expect{|
Line 1, characters 23-31:
1 | type t = Foo of int @@ portable nonportable
                           ^^^^^^^^
Warning 213: This portability is overriden by nonportable later.

Line 1, characters 32-43:
1 | type t = Foo of int @@ portable nonportable
                                    ^^^^^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

type t = Foo of int
|}]

(**************************************************************************)
(* Part 6: Implied modalities *)
(**************************************************************************)

(* global implies aliased *)
module type S = sig
  val x : int @@ global aliased
end
[%%expect{|
Line 2, characters 24-31:
2 |   val x : int @@ global aliased
                            ^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

module type S = sig val x : int @@ global end
|}]

(* stateless implies portable *)
module type S = sig
  val x : int @@ stateless portable
end
[%%expect{|
Line 2, characters 27-35:
2 |   val x : int @@ stateless portable
                               ^^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

module type S = sig val x : int @@ stateless end
|}]

(* In sig @@ global, writing global aliased is redundant *)
module type S = sig @@ global
  val x : int @@ global aliased
end
[%%expect{|
Line 2, characters 17-23:
2 |   val x : int @@ global aliased
                     ^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

Line 2, characters 24-31:
2 |   val x : int @@ global aliased
                            ^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

module type S = sig val x : int @@ global end
|}]

(**************************************************************************)
(* Part 7: Modalities on with bounds *)
(**************************************************************************)

(* Identity modality on with bounds is redundant *)
type 'a t : immediate with 'a @@ nonportable
[%%expect{|
Line 1, characters 33-44:
1 | type 'a t : immediate with 'a @@ nonportable
                                     ^^^^^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

type 'a t : immediate with 'a
|}]

(* Implied modality on with bounds is redundant *)
type 'a t : immediate with 'a @@ global aliased
[%%expect{|
Line 1, characters 40-47:
1 | type 'a t : immediate with 'a @@ global aliased
                                            ^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

type 'a t : immediate with 'a @@ global
|}]

(* Multiple with bounds, one redundant *)
type ('a, 'b) t : immediate with 'a @@ stateful with 'b @@ aliased
[%%expect{|
Line 1, characters 39-47:
1 | type ('a, 'b) t : immediate with 'a @@ stateful with 'b @@ aliased
                                           ^^^^^^^^
Warning 217 [redundant-modality]: This modality is redundant.

type ('a, 'b) t : immediate with 'a with 'b @@ aliased
|}]

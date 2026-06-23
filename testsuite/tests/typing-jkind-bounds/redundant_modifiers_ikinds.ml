(* TEST
    flags = "-extension mode_alpha";
    expect;
*)

(* This file tests the behavior of redundant or conflicting modifiers.

   Unlike modalities, multiple modifiers on the same axis are an error rather
   than a warning. Warning 211 is triggered by a modifier that has no effect.
*)

(**************************************************************************)
(* Part 1: Two modifiers on the same axis *)
(**************************************************************************)

type t : value mod portable nonportable
[%%expect{|
Line 1, characters 28-39:
1 | type t : value mod portable nonportable
                                ^^^^^^^^^^^
Error: The portability axis has already been specified.
|}]

type t : value mod contended uncontended
[%%expect{|
Line 1, characters 29-40:
1 | type t : value mod contended uncontended
                                 ^^^^^^^^^^^
Error: The contention axis has already been specified.
|}]

type t : value mod portable shareable
[%%expect{|
Line 1, characters 28-37:
1 | type t : value mod portable shareable
                                ^^^^^^^^^
Error: The portability axis has already been specified.
|}]

type t : value mod static static
[%%expect{|
Line 1, characters 26-32:
1 | type t : value mod static static
                              ^^^^^^
Error: The staticity axis has already been specified.
|}]

(**************************************************************************)
(* Part 2: Modifiers that match the default bounds *)
(**************************************************************************)

type t : value mod local
[%%expect{|
Line 1, characters 19-24:
1 | type t : value mod local
                       ^^^^^
Warning 211 [redundant-modifier]: This modifier is redundant.

type t
|}]

type t : value mod unique
[%%expect{|
Line 1, characters 19-25:
1 | type t : value mod unique
                       ^^^^^^
Warning 211 [redundant-modifier]: This modifier is redundant.

type t
|}]

type t : value mod uncontended
[%%expect{|
Line 1, characters 19-30:
1 | type t : value mod uncontended
                       ^^^^^^^^^^^
Warning 211 [redundant-modifier]: This modifier is redundant.

type t
|}]

type t : value mod internal
[%%expect{|
Line 1, characters 19-27:
1 | type t : value mod internal
                       ^^^^^^^^
Warning 211 [redundant-modifier]: This modifier is redundant.

type t
|}]

type t : value mod static
[%%expect{|
Line 1, characters 19-25:
1 | type t : value mod static
                       ^^^^^^
Warning 211 [redundant-modifier]: This modifier is redundant.

type t
|}]

(**************************************************************************)
(* Part 3: Implied modifiers, in either order *)
(**************************************************************************)

type t : value mod global aliased
[%%expect{|
Line 1, characters 26-33:
1 | type t : value mod global aliased
                              ^^^^^^^
Warning 211 [redundant-modifier]: This modifier is redundant.

type t : value mod global
|}]

type t : value mod aliased global
[%%expect{|
Line 1, characters 19-26:
1 | type t : value mod aliased global
                       ^^^^^^^
Warning 211 [redundant-modifier]: This modifier is redundant.

type t : value mod global
|}]

type t : value mod stateless portable
[%%expect{|
Line 1, characters 29-37:
1 | type t : value mod stateless portable
                                 ^^^^^^^^
Warning 211 [redundant-modifier]: This modifier is redundant.

type t : value mod stateless
|}]

type t : value mod portable stateless
[%%expect{|
Line 1, characters 19-27:
1 | type t : value mod portable stateless
                       ^^^^^^^^
Warning 211 [redundant-modifier]: This modifier is redundant.

type t : value mod stateless
|}]

type t : value mod immutable contended
[%%expect{|
Line 1, characters 29-38:
1 | type t : value mod immutable contended
                                 ^^^^^^^^^
Warning 211 [redundant-modifier]: This modifier is redundant.

type t : value mod immutable
|}]

type t : value mod contended immutable
[%%expect{|
Line 1, characters 19-28:
1 | type t : value mod contended immutable
                       ^^^^^^^^^
Warning 211 [redundant-modifier]: This modifier is redundant.

type t : value mod immutable
|}]

(**************************************************************************)
(* Part 4: Conflicting modifiers, in either order *)
(**************************************************************************)

type t : value mod global unique
[%%expect{|
Line 1, characters 19-32:
1 | type t : value mod global unique
                       ^^^^^^^^^^^^^
Error: The modifier "global" can't be used together with "unique"
|}]

type t : value mod unique global
[%%expect{|
Line 1, characters 19-32:
1 | type t : value mod unique global
                       ^^^^^^^^^^^^^
Error: The modifier "global" can't be used together with "unique"
|}]

(**************************************************************************)
(* Part 5: [everything] *)
(**************************************************************************)

type t : value mod everything
[%%expect{|
type t : value mod everything
|}]

(* [everything] conflicts with modal modifiers on either side. *)

type t : value mod everything portable
[%%expect{|
Line 1, characters 30-38:
1 | type t : value mod everything portable
                                  ^^^^^^^^
Error: The portability axis has already been specified.
|}]

type t : value mod portable everything
[%%expect{|
Line 1, characters 28-38:
1 | type t : value mod portable everything
                                ^^^^^^^^^^
Error: The portability axis has already been specified.
|}]

(* ... and with externality modifiers. *)

type t : value mod everything external_
[%%expect{|
Line 1, characters 30-39:
1 | type t : value mod everything external_
                                  ^^^^^^^^^
Error: The externality axis has already been specified.
|}]

type t : value mod external_ everything
[%%expect{|
Line 1, characters 29-39:
1 | type t : value mod external_ everything
                                 ^^^^^^^^^^
Error: The externality axis has already been specified.
|}]

type t : value mod everything everything
[%%expect{|
Line 1, characters 30-40:
1 | type t : value mod everything everything
                                  ^^^^^^^^^^
Error: The externality axis has already been specified.
|}]

(* [everything] does not specify staticity, so [static] is allowed (but
   redundant) on either side. *)

type t : value mod everything static
[%%expect{|
Line 1, characters 30-36:
1 | type t : value mod everything static
                                  ^^^^^^
Warning 211 [redundant-modifier]: This modifier is redundant.

type t : value mod everything
|}]

type t : value mod static everything
[%%expect{|
Line 1, characters 19-25:
1 | type t : value mod static everything
                       ^^^^^^
Warning 211 [redundant-modifier]: This modifier is redundant.

type t : value mod everything
|}]

(* [everything] does not conflict with nullability or separability. *)

type t : value mod everything non_float
[%%expect{|
type t : value mod everything non_float
|}]

(**************************************************************************)
(* Part 6: Nonmodal modifiers *)
(**************************************************************************)

type t : value mod external_ external64
[%%expect{|
Line 1, characters 29-39:
1 | type t : value mod external_ external64
                                 ^^^^^^^^^^
Error: The externality axis has already been specified.
|}]

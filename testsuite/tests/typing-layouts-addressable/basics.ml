(* TEST
 expect;
*)

(* Tests for the [addressable] kind operator. An addressable kind is one
   whose types store all of their information in the data portion of a
   block when boxed. If [k] is addressable, then [k addressable] = [k]. *)

(* [addressable] on base layouts that are not already addressable makes a
   new kind, distinct from the base layout. *)

type t : bits8 addressable
[%%expect{|
type t : bits8 addressable
|}]

type t : bits16 addressable
[%%expect{|
type t : bits16 addressable
|}]

type t : bits32 addressable
[%%expect{|
type t : bits32 addressable
|}]

type t : float32 addressable
[%%expect{|
type t : float32 addressable
|}]

type t : float64 addressable
[%%expect{|
type t : float64 addressable
|}]

type t : void addressable
[%%expect{|
type t : void addressable
|}]

type t : untagged_immediate addressable
[%%expect{|
type t : untagged_immediate addressable
|}]

(* [any addressable] is the top of all addressable kinds. *)

type t : any addressable
[%%expect{|
type t : any addressable
|}]

(* [addressable] on a product whose components are not all addressable. *)

type t : (bits8 & value) addressable
[%%expect{|
type t : (bits8 & value) addressable
|}]

(* [addressable] on an already-addressable kind is the identity. *)

type t : value addressable
[%%expect{|
Line 1, characters 15-26:
1 | type t : value addressable
                   ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "value".

type t
|}]

type t : bits64 addressable
[%%expect{|
Line 1, characters 16-27:
1 | type t : bits64 addressable
                    ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits64".

type t : bits64
|}]

type t : word addressable
[%%expect{|
Line 1, characters 14-25:
1 | type t : word addressable
                  ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "word".

type t : word
|}]

type t : vec128 addressable
[%%expect{|
Line 1, characters 16-27:
1 | type t : vec128 addressable
                    ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "vec128".

type t : vec128
|}]

type t : (value & bits64) addressable
[%%expect{|
Line 1, characters 26-37:
1 | type t : (value & bits64) addressable
                              ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "value & bits64".

type t : value & bits64
|}]

(* Double application: the second application is the identity. *)

type t : bits8 addressable addressable
[%%expect{|
Line 1, characters 27-38:
1 | type t : bits8 addressable addressable
                               ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 addressable".

type t : bits8 addressable
|}]

(* [addressable] combines with scannable axes. *)

type t : value non_null addressable
[%%expect{|
Line 1, characters 15-23:
1 | type t : value non_null addressable
                   ^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "value".

Line 1, characters 24-35:
1 | type t : value non_null addressable
                            ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "value non_null".

type t
|}]

type t : any non_null addressable
[%%expect{|
type t : any addressable non_null
|}]

(* [addressable] is a kind operator, not a [mod] modifier. *)

type t : value mod addressable
[%%expect{|
Line 1, characters 19-30:
1 | type t : value mod addressable
                       ^^^^^^^^^^^
Error: Unrecognized modifier addressable.
|}]

(* Unknown kind modifiers still error. *)

type t : value addressablee
[%%expect{|
Line 1, characters 15-27:
1 | type t : value addressablee
                   ^^^^^^^^^^^^
Error: Unknown kind modifier addressablee
|}]

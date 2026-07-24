(* TEST
 expect;
*)

(* Tests for the [addressable] kind operator. An addressable kind is one
   whose types store all of their information in the data portion of a
   block when boxed. If [k] is addressable, then [k addressable] = [k]. *)

(* CR rtjoa: the [addressable] operator is not implemented yet, so the
   outputs in this directory show "Unknown kind modifier" errors. *)

(* [addressable] on base layouts that are not already addressable makes a
   new kind, distinct from the base layout. *)

type t : bits8 addressable
[%%expect{|
Line 1, characters 15-26:
1 | type t : bits8 addressable
                   ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t : bits16 addressable
[%%expect{|
Line 1, characters 16-27:
1 | type t : bits16 addressable
                    ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t : bits32 addressable
[%%expect{|
Line 1, characters 16-27:
1 | type t : bits32 addressable
                    ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t : float32 addressable
[%%expect{|
Line 1, characters 17-28:
1 | type t : float32 addressable
                     ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t : float64 addressable
[%%expect{|
Line 1, characters 17-28:
1 | type t : float64 addressable
                     ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t : void addressable
[%%expect{|
Line 1, characters 14-25:
1 | type t : void addressable
                  ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t : untagged_immediate addressable
[%%expect{|
Line 1, characters 28-39:
1 | type t : untagged_immediate addressable
                                ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* [any addressable] is the top of all addressable kinds. *)

type t : any addressable
[%%expect{|
Line 1, characters 13-24:
1 | type t : any addressable
                 ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* [addressable] on a product whose components are not all addressable. *)

type t : (bits8 & value) addressable
[%%expect{|
Line 1, characters 25-36:
1 | type t : (bits8 & value) addressable
                             ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* [addressable] on an already-addressable kind is the identity. *)

type t : value addressable
[%%expect{|
Line 1, characters 15-26:
1 | type t : value addressable
                   ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t : bits64 addressable
[%%expect{|
Line 1, characters 16-27:
1 | type t : bits64 addressable
                    ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t : word addressable
[%%expect{|
Line 1, characters 14-25:
1 | type t : word addressable
                  ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t : vec128 addressable
[%%expect{|
Line 1, characters 16-27:
1 | type t : vec128 addressable
                    ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t : (value & bits64) addressable
[%%expect{|
Line 1, characters 26-37:
1 | type t : (value & bits64) addressable
                              ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* Double application: the second application is the identity. *)

type t : bits8 addressable addressable
[%%expect{|
Line 1, characters 15-26:
1 | type t : bits8 addressable addressable
                   ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* [addressable] combines with scannable axes. *)

type t : value non_null addressable
[%%expect{|
Line 1, characters 24-35:
1 | type t : value non_null addressable
                            ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

type t : any non_null addressable
[%%expect{|
Line 1, characters 22-33:
1 | type t : any non_null addressable
                          ^^^^^^^^^^^
Error: Unknown kind modifier addressable
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

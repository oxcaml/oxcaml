(* TEST
 expect;
*)

(* Warnings for the [addressable] kind operator. *)

(* Applying [addressable] to an already-addressable kind is the identity
   and warns (warning 183). *)

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

type t : (value & bits64) addressable
[%%expect{|
Line 1, characters 26-37:
1 | type t : (value & bits64) addressable
                              ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "value & bits64".

type t : value & bits64
|}]

type t : bits8 addressable addressable
[%%expect{|
Line 1, characters 27-38:
1 | type t : bits8 addressable addressable
                               ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "bits8 addressable".

type t : bits8 addressable
|}]

(* No warning 184: [addressable] is meaningful on non-scannable layouts. *)

type t : bits8 addressable
[%%expect{|
type t : bits8 addressable
|}]

type t : (bits8 & value) addressable
[%%expect{|
type t : (bits8 & value) addressable
|}]

(* Warning 184 still fires for scannable axes on non-scannable layouts... *)

type t : bits8 non_pointer
[%%expect{|
Line 1, characters 9-26:
1 | type t : bits8 non_pointer
             ^^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_pointer" have no effect on the kind "bits8".

type t : bits8
|}]

(* ...including when mixed with a meaningful [addressable]. *)

type t : bits8 non_pointer addressable
[%%expect{|
Line 1, characters 9-38:
1 | type t : bits8 non_pointer addressable
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_pointer" have no effect on the kind "bits8".

type t : bits8 addressable
|}]

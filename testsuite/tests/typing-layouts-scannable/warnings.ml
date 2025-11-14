(* TEST
 flags = "-extension layouts_alpha -w +183..185";
 expect;
*)

type t : value non_pointerrrrr
[%%expect{|
Line 1, characters 15-30:
1 | type t : value non_pointerrrrr
                   ^^^^^^^^^^^^^^^
Error: Unknown kind modifier non_pointerrrrr
|}]

type t : non_pointer value
[%%expect{|
Line 1, characters 9-26:
1 | type t : non_pointer value
             ^^^^^^^^^^^^^^^^^
Error: Unknown layout non_pointer value
|}]

type t : value non_pointer = int [@@immediate]
[%%expect{|
Line 1, characters 0-46:
1 | type t : value non_pointer = int [@@immediate]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type declaration's layout can be given at most once.
       This declaration has an layout annotation (value non_pointer) and a layout attribute ([@@immediate]).
|}]

(* CR layouts-scannable: The following errors should only print ONCE.
   They are disabled by default because of the triple printing
   but enabled locally in this test file. Once this is fixed, adjust this! *)

type t : value maybe_pointer
[%%expect{|
Line 1, characters 15-28:
1 | type t : value maybe_pointer
                   ^^^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This modifier is already implied by the layout "value".

Line 1, characters 15-28:
1 | type t : value maybe_pointer
                   ^^^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This modifier is already implied by the layout "value".

Line 1, characters 15-28:
1 | type t : value maybe_pointer
                   ^^^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This modifier is already implied by the layout "value".

type t
|}]

(* CR layouts-scannable: This should give a warning for a redundant annotation.
   This logic will come in the _following_ PR! *)
type t : immediate non_pointer
[%%expect{|
type t : immediate non_pointer
|}]

type t : value maybe_pointer non_pointer
[%%expect{|
Line 1, characters 15-28:
1 | type t : value maybe_pointer non_pointer
                   ^^^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "non_pointer" later.

Line 1, characters 15-28:
1 | type t : value maybe_pointer non_pointer
                   ^^^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "non_pointer" later.

Line 1, characters 15-28:
1 | type t : value maybe_pointer non_pointer
                   ^^^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "non_pointer" later.

type t : value non_pointer
|}]

type t : value non_pointer maybe_pointer
[%%expect{|
Line 1, characters 15-26:
1 | type t : value non_pointer maybe_pointer
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "maybe_pointer" later.

Line 1, characters 27-40:
1 | type t : value non_pointer maybe_pointer
                               ^^^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This modifier is already implied by the layout "value".

Line 1, characters 15-26:
1 | type t : value non_pointer maybe_pointer
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "maybe_pointer" later.

Line 1, characters 27-40:
1 | type t : value non_pointer maybe_pointer
                               ^^^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This modifier is already implied by the layout "value".

Line 1, characters 15-26:
1 | type t : value non_pointer maybe_pointer
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "maybe_pointer" later.

Line 1, characters 27-40:
1 | type t : value non_pointer maybe_pointer
                               ^^^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This modifier is already implied by the layout "value".

type t
|}]

type t : value non_pointer non_pointer
[%%expect{|
Line 1, characters 15-26:
1 | type t : value non_pointer non_pointer
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "non_pointer" later.

Line 1, characters 15-26:
1 | type t : value non_pointer non_pointer
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "non_pointer" later.

Line 1, characters 15-26:
1 | type t : value non_pointer non_pointer
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "non_pointer" later.

type t : value non_pointer
|}]

type t : value non_pointer maybe_pointer non_pointer
[%%expect{|
Line 1, characters 27-40:
1 | type t : value non_pointer maybe_pointer non_pointer
                               ^^^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "non_pointer" later.

Line 1, characters 15-26:
1 | type t : value non_pointer maybe_pointer non_pointer
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "non_pointer" later.

Line 1, characters 27-40:
1 | type t : value non_pointer maybe_pointer non_pointer
                               ^^^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "non_pointer" later.

Line 1, characters 15-26:
1 | type t : value non_pointer maybe_pointer non_pointer
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "non_pointer" later.

Line 1, characters 27-40:
1 | type t : value non_pointer maybe_pointer non_pointer
                               ^^^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "non_pointer" later.

Line 1, characters 15-26:
1 | type t : value non_pointer maybe_pointer non_pointer
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "non_pointer" later.

type t : value non_pointer
|}]

type t : void non_pointer
[%%expect{|
Line 1, characters 9-25:
1 | type t : void non_pointer
             ^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_pointer" have no effect on the layout "void".

Line 1, characters 9-25:
1 | type t : void non_pointer
             ^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_pointer" have no effect on the layout "void".

Line 1, characters 9-25:
1 | type t : void non_pointer
             ^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_pointer" have no effect on the layout "void".

type t : void
|}]

type t : void non_pointer maybe_pointer
[%%expect{|
Line 1, characters 14-25:
1 | type t : void non_pointer maybe_pointer
                  ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "maybe_pointer" later.

Line 1, characters 9-39:
1 | type t : void non_pointer maybe_pointer
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_pointer maybe_pointer" have no effect on the layout "void".

Line 1, characters 14-25:
1 | type t : void non_pointer maybe_pointer
                  ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "maybe_pointer" later.

Line 1, characters 9-39:
1 | type t : void non_pointer maybe_pointer
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_pointer maybe_pointer" have no effect on the layout "void".

Line 1, characters 14-25:
1 | type t : void non_pointer maybe_pointer
                  ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This modifier is overridden by "maybe_pointer" later.

Line 1, characters 9-39:
1 | type t : void non_pointer maybe_pointer
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_pointer maybe_pointer" have no effect on the layout "void".

type t : void
|}]

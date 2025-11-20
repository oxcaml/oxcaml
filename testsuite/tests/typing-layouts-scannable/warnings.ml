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

type t : value separable
[%%expect{|
Line 1, characters 15-24:
1 | type t : value separable
                   ^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier is already implied by the kind "value".

Line 1, characters 15-24:
1 | type t : value separable
                   ^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier is already implied by the kind "value".

Line 1, characters 15-24:
1 | type t : value separable
                   ^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier is already implied by the kind "value".

type t
|}]

(* CR layouts-scannable: This should give a warning for a redundant annotation.
   This logic will come in a _following_ PR! *)
type t : immediate non_pointer
[%%expect{|
Line 1, characters 19-30:
1 | type t : immediate non_pointer
                       ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier is already implied by the kind "immediate".

Line 1, characters 19-30:
1 | type t : immediate non_pointer
                       ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier is already implied by the kind "immediate".

Line 1, characters 19-30:
1 | type t : immediate non_pointer
                       ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier is already implied by the kind "immediate".

type t : immediate
|}]

type t : value maybe_separable non_pointer
[%%expect{|
Line 1, characters 15-30:
1 | type t : value maybe_separable non_pointer
                   ^^^^^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "non_pointer" later.

Line 1, characters 15-30:
1 | type t : value maybe_separable non_pointer
                   ^^^^^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "non_pointer" later.

Line 1, characters 15-30:
1 | type t : value maybe_separable non_pointer
                   ^^^^^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "non_pointer" later.

type t : value non_pointer
|}]

type t : value non_pointer separable
[%%expect{|
Line 1, characters 15-26:
1 | type t : value non_pointer separable
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "separable" later.

Line 1, characters 27-36:
1 | type t : value non_pointer separable
                               ^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier is already implied by the kind "value".

Line 1, characters 15-26:
1 | type t : value non_pointer separable
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "separable" later.

Line 1, characters 27-36:
1 | type t : value non_pointer separable
                               ^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier is already implied by the kind "value".

Line 1, characters 15-26:
1 | type t : value non_pointer separable
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "separable" later.

Line 1, characters 27-36:
1 | type t : value non_pointer separable
                               ^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier is already implied by the kind "value".

type t
|}]

type t : value non_pointer non_pointer
[%%expect{|
Line 1, characters 15-26:
1 | type t : value non_pointer non_pointer
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "non_pointer" later.

Line 1, characters 15-26:
1 | type t : value non_pointer non_pointer
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "non_pointer" later.

Line 1, characters 15-26:
1 | type t : value non_pointer non_pointer
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "non_pointer" later.

type t : value non_pointer
|}]

type t : value non_pointer maybe_separable non_pointer
[%%expect{|
Line 1, characters 27-42:
1 | type t : value non_pointer maybe_separable non_pointer
                               ^^^^^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "non_pointer" later.

Line 1, characters 15-26:
1 | type t : value non_pointer maybe_separable non_pointer
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "non_pointer" later.

Line 1, characters 27-42:
1 | type t : value non_pointer maybe_separable non_pointer
                               ^^^^^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "non_pointer" later.

Line 1, characters 15-26:
1 | type t : value non_pointer maybe_separable non_pointer
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "non_pointer" later.

Line 1, characters 27-42:
1 | type t : value non_pointer maybe_separable non_pointer
                               ^^^^^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "non_pointer" later.

Line 1, characters 15-26:
1 | type t : value non_pointer maybe_separable non_pointer
                   ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "non_pointer" later.

type t : value non_pointer
|}]

type t : void non_pointer
[%%expect{|
Line 1, characters 9-25:
1 | type t : void non_pointer
             ^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_pointer" have no effect on the kind "void".

Line 1, characters 9-25:
1 | type t : void non_pointer
             ^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_pointer" have no effect on the kind "void".

Line 1, characters 9-25:
1 | type t : void non_pointer
             ^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_pointer" have no effect on the kind "void".

type t : void
|}]

type t : void non_pointer maybe_separable
[%%expect{|
Line 1, characters 14-25:
1 | type t : void non_pointer maybe_separable
                  ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "maybe_separable" later.

Line 1, characters 9-41:
1 | type t : void non_pointer maybe_separable
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_pointer maybe_separable" have no effect on the kind "void".

Line 1, characters 14-25:
1 | type t : void non_pointer maybe_separable
                  ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "maybe_separable" later.

Line 1, characters 9-41:
1 | type t : void non_pointer maybe_separable
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_pointer maybe_separable" have no effect on the kind "void".

Line 1, characters 14-25:
1 | type t : void non_pointer maybe_separable
                  ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "maybe_separable" later.

Line 1, characters 9-41:
1 | type t : void non_pointer maybe_separable
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_pointer maybe_separable" have no effect on the kind "void".

type t : void
|}]

type t : void non_pointer & value non_pointer non_pointer mod global with int
[%%expect{|
Line 1, characters 9-25:
1 | type t : void non_pointer & value non_pointer non_pointer mod global with int
             ^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_pointer" have no effect on the kind "void".

Line 1, characters 34-45:
1 | type t : void non_pointer & value non_pointer non_pointer mod global with int
                                      ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "non_pointer" later.

Line 1, characters 9-25:
1 | type t : void non_pointer & value non_pointer non_pointer mod global with int
             ^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_pointer" have no effect on the kind "void".

Line 1, characters 34-45:
1 | type t : void non_pointer & value non_pointer non_pointer mod global with int
                                      ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "non_pointer" later.

Line 1, characters 9-25:
1 | type t : void non_pointer & value non_pointer non_pointer mod global with int
             ^^^^^^^^^^^^^^^^
Warning 184 [ignored-kind-modifier]: The kind modifier(s) "non_pointer" have no effect on the kind "void".

Line 1, characters 34-45:
1 | type t : void non_pointer & value non_pointer non_pointer mod global with int
                                      ^^^^^^^^^^^
Warning 185 [overridden-kind-modifier]: This kind modifier is overridden by "non_pointer" later.

type t : void mod global & value non_pointer mod global
|}]

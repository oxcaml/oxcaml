(* TEST
 flags = "-extension layouts_alpha -w +184+185";
 expect;
*)

type t : value non_pointerrrrr
[%%expect{|
Line 1, characters 15-30:
1 | type t : value non_pointerrrrr
                   ^^^^^^^^^^^^^^^
Error: Unknown scannable axis non_pointerrrrr
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

(* CR zeisbach: this should give a warning for a redundant annotation.
   this logic will come in the _following_ PR! *)
type t : immediate non_pointer
[%%expect{|
type t : immediate
|}]

(* CR zeisbach: the following errors should only print ONCE.
   they are disabled by default because of the triple printing
   and enabled locally in this test file. once this is fixed, adjust this *)

type t : value non_pointer maybe_pointer
[%%expect{|
Line 1, characters 27-40:
1 | type t : value non_pointer maybe_pointer
                               ^^^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 27-40:
1 | type t : value non_pointer maybe_pointer
                               ^^^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 27-40:
1 | type t : value non_pointer maybe_pointer
                               ^^^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

type t
|}]

type t : value non_pointer non_pointer
[%%expect{|
Line 1, characters 27-38:
1 | type t : value non_pointer non_pointer
                               ^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 27-38:
1 | type t : value non_pointer non_pointer
                               ^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 27-38:
1 | type t : value non_pointer non_pointer
                               ^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

type t
|}]

type t : value maybe_pointer non_pointer maybe_pointer
[%%expect{|
Line 1, characters 29-40:
1 | type t : value maybe_pointer non_pointer maybe_pointer
                                 ^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 41-54:
1 | type t : value maybe_pointer non_pointer maybe_pointer
                                             ^^^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 29-40:
1 | type t : value maybe_pointer non_pointer maybe_pointer
                                 ^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 41-54:
1 | type t : value maybe_pointer non_pointer maybe_pointer
                                             ^^^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 29-40:
1 | type t : value maybe_pointer non_pointer maybe_pointer
                                 ^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 41-54:
1 | type t : value maybe_pointer non_pointer maybe_pointer
                                             ^^^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

type t
|}]

type t : void non_pointer
[%%expect{|
Line 1, characters 9-25:
1 | type t : void non_pointer
             ^^^^^^^^^^^^^^^^
Warning 184 [ignored-scannable-axes]: The specified scannable axes are meaningless, since they apply to the layout void

Line 1, characters 9-25:
1 | type t : void non_pointer
             ^^^^^^^^^^^^^^^^
Warning 184 [ignored-scannable-axes]: The specified scannable axes are meaningless, since they apply to the layout void

Line 1, characters 9-25:
1 | type t : void non_pointer
             ^^^^^^^^^^^^^^^^
Warning 184 [ignored-scannable-axes]: The specified scannable axes are meaningless, since they apply to the layout void

type t : void
|}]

type t : void non_pointer maybe_pointer
[%%expect{|
Line 1, characters 26-39:
1 | type t : void non_pointer maybe_pointer
                              ^^^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 9-39:
1 | type t : void non_pointer maybe_pointer
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 184 [ignored-scannable-axes]: The specified scannable axes are meaningless, since they apply to the layout void

Line 1, characters 26-39:
1 | type t : void non_pointer maybe_pointer
                              ^^^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 9-39:
1 | type t : void non_pointer maybe_pointer
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 184 [ignored-scannable-axes]: The specified scannable axes are meaningless, since they apply to the layout void

Line 1, characters 26-39:
1 | type t : void non_pointer maybe_pointer
                              ^^^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 9-39:
1 | type t : void non_pointer maybe_pointer
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 184 [ignored-scannable-axes]: The specified scannable axes are meaningless, since they apply to the layout void

type t : void
|}]

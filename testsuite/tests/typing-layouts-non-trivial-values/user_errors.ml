(* TEST
 flags = "-extension layouts_alpha -w +184+185";
 expect;
*)

(* CR zeisbach: once the scannable axes can actually be parsed into a
   [Scannable_axes.t], then more tests should be added here, and they
   should be moved into [source_jane_street.ml] since they will round-trip *)
(* CR zeisbach: once annotations aren't being dropped on the floor, the printing
   in the good cases should include more layout information *)
type t : value non_pointer = int
[%%expect{|
type t = int
|}]

type t : immutable_data non_pointer = int
[%%expect{|
type t = int
|}]

type ('a : any non_pointer, 'b : any maybe_pointer, 'c : any) t;;
[%%expect{|
type ('a : any, 'b : any, 'c : any) t
|}]

type t : non_pointer value = int
(* CR zeisbach: should this pretty-printing be changed? maybe it will just
   change automatically in the next PR with more printing.
   if not, try to do this... *)
[%%expect{|
Line 1, characters 9-26:
1 | type t : non_pointer value = int
             ^^^^^^^^^^^^^^^^^
Error: Unknown layout non_pointer
       value
|}]

(* CR zeisbach: this should give a warning for a redundant annotation.
   this logic will come in the _following_ PR! *)
type t : immediate non_pointer = int
[%%expect{|
type t = int
|}]

(* CR zeisbach: the following errors should only print ONCE.
   they are disabled by default because of the triple printing
   and enabled locally in this test file. once this is fixed, adjust this *)

type t : value non_pointer maybe_pointer = int
[%%expect{|
Line 1, characters 27-40:
1 | type t : value non_pointer maybe_pointer = int
                               ^^^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 27-40:
1 | type t : value non_pointer maybe_pointer = int
                               ^^^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 27-40:
1 | type t : value non_pointer maybe_pointer = int
                               ^^^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

type t = int
|}]

type t : value non_pointer non_pointer = int
[%%expect{|
Line 1, characters 27-38:
1 | type t : value non_pointer non_pointer = int
                               ^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 27-38:
1 | type t : value non_pointer non_pointer = int
                               ^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 27-38:
1 | type t : value non_pointer non_pointer = int
                               ^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

type t = int
|}]

type t : value maybe_pointer non_pointer maybe_pointer = int
[%%expect{|
Line 1, characters 29-40:
1 | type t : value maybe_pointer non_pointer maybe_pointer = int
                                 ^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 41-54:
1 | type t : value maybe_pointer non_pointer maybe_pointer = int
                                             ^^^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 29-40:
1 | type t : value maybe_pointer non_pointer maybe_pointer = int
                                 ^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 41-54:
1 | type t : value maybe_pointer non_pointer maybe_pointer = int
                                             ^^^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 29-40:
1 | type t : value maybe_pointer non_pointer maybe_pointer = int
                                 ^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

Line 1, characters 41-54:
1 | type t : value maybe_pointer non_pointer maybe_pointer = int
                                             ^^^^^^^^^^^^^
Warning 185 [duplicated-scannable-axis]: The pointerness axis has already been specified.

type t = int
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

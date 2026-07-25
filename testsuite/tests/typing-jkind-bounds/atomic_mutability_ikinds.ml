(* TEST
   expect;
*)

(**** atomic vs non-atomic mutable fields ****)

(* Records: all atomic fields are sync_data. *)
type t : sync_data =
  { mutable x : int [@atomic]; mutable y : int [@atomic] }
[%%expect {|
type t = { mutable x : int [@atomic]; mutable y : int [@atomic]; }
|}]

(* Records: atomic fields are not immutable_data. *)
type t : immutable_data = { mutable x : int [@atomic] }
[%%expect {|
Line 1, characters 0-55:
1 | type t : immutable_data = { mutable x : int [@atomic] }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type definition does not satisfy its kind annotation immutable_data,
       because atomic mutable fields are not mod immutable.
|}]

(* Records: any non-atomic mutable field is not sync_data. *)
type t : sync_data = { mutable x : int }
[%%expect {|
Line 1, characters 0-40:
1 | type t : sync_data = { mutable x : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type definition does not satisfy its kind annotation sync_data,
       because mutable fields are not mod contended.
|}]

type t : sync_data =
  { mutable x : int; mutable y : int [@atomic] }
[%%expect {|
Lines 1-2, characters 0-48:
1 | type t : sync_data =
2 |   { mutable x : int; mutable y : int [@atomic] }
Error: This type definition does not satisfy its kind annotation sync_data,
       because mutable fields are not mod contended.
|}]

(* Two offending fields: identical subjects merge, distinct ones do not. *)
type t : immutable_data =
  { mutable x : int [@atomic]; mutable y : int [@atomic] }
[%%expect {|
Lines 1-2, characters 0-58:
1 | type t : immutable_data =
2 |   { mutable x : int [@atomic]; mutable y : int [@atomic] }
Error: This type definition does not satisfy its kind annotation immutable_data,
       because atomic mutable fields are not mod immutable.
|}]

type t : immutable_data = { mutable x : int [@atomic]; mutable y : int }
[%%expect {|
Line 1, characters 0-72:
1 | type t : immutable_data = { mutable x : int [@atomic]; mutable y : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type definition does not satisfy its kind annotation immutable_data,
       because
       - atomic mutable fields are not mod immutable
       - mutable fields are not mod immutable
|}]

type t : immutable_data = { mutable x : int; mutable y : int }
[%%expect {|
Line 1, characters 0-62:
1 | type t : immutable_data = { mutable x : int; mutable y : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type definition does not satisfy its kind annotation immutable_data,
       because mutable fields are not mod immutable.
|}]

(* Variants: atomic record payloads are sync_data. *)
type t : sync_data = A of { mutable x : int [@atomic] } | B
[%%expect {|
type t = A of { mutable x : int [@atomic]; } | B
|}]

(* Variants: any non-atomic mutable field is not sync_data. *)
type t : sync_data = A of { mutable x : int } | B
[%%expect {|
Line 1, characters 0-49:
1 | type t : sync_data = A of { mutable x : int } | B
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type definition does not satisfy its kind annotation sync_data,
       because mutable fields are not mod contended.
|}]

type t : sync_data =
  A of { mutable x : int; mutable y : int [@atomic] }
[%%expect {|
Lines 1-2, characters 0-53:
1 | type t : sync_data =
2 |   A of { mutable x : int; mutable y : int [@atomic] }
Error: This type definition does not satisfy its kind annotation sync_data,
       because mutable fields are not mod contended.
|}]

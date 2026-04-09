(* TEST
   flags = "-ikinds";
   expect;
*)

(**** atomic vs non-atomic mutable fields ****)

(* Records: all atomic fields are sync_data. *)
type t : sync_data =
  { mutable x : int [@atomic]; mutable y : int [@atomic] }
[%%expect {|
type t = { mutable x : int [@atomic]; mutable y : int [@atomic]; }
|}]

(* Records: any non-atomic mutable field is not sync_data. *)
type t : sync_data = { mutable x : int }
[%%expect {|
Line 1, characters 0-40:
1 | type t : sync_data = { mutable x : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of sync_data
         because of the annotation on the declaration of the type t.
|}]

type t : sync_data =
  { mutable x : int; mutable y : int [@atomic] }
[%%expect {|
Lines 1-2, characters 0-48:
1 | type t : sync_data =
2 |   { mutable x : int; mutable y : int [@atomic] }
Error: The kind of type "t" is mutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of sync_data
         because of the annotation on the declaration of the type t.
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
Error: The kind of type "t" is mutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of sync_data
         because of the annotation on the declaration of the type t.
|}]

type t : sync_data =
  A of { mutable x : int; mutable y : int [@atomic] }
[%%expect {|
Lines 1-2, characters 0-53:
1 | type t : sync_data =
2 |   A of { mutable x : int; mutable y : int [@atomic] }
Error: The kind of type "t" is mutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of sync_data
         because of the annotation on the declaration of the type t.
|}]

(* TEST
   expect;
*)

(* Regression test: immutable record fields contribute an untracked
   [Axis_lattice.immediate], so [provenance_residuals] gives up and the error
   falls back. Only [mod dynamic] is affected: [immediate] is bottom on every
   other axis. *)

type t : value mod dynamic = { mutable x : int }
[%%expect {|
Line 1, characters 0-48:
1 | type t : value mod dynamic = { mutable x : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type definition does not satisfy its kind annotation value mod dynamic,
       because
       - boxed records are not mod dynamic
       - mutable fields are not mod dynamic
|}]

(* Same, plus one immutable field. *)
type t : value mod dynamic = { mutable x : int; y : int }
[%%expect {|
Line 1, characters 0-57:
1 | type t : value mod dynamic = { mutable x : int; y : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod dynamic
         because of the annotation on the declaration of the type t.
|}]

type t : value mod dynamic = { x : int }
[%%expect {|
Line 1, characters 0-40:
1 | type t : value mod dynamic = { x : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod dynamic
         because of the annotation on the declaration of the type t.
|}]

type t : value mod dynamic = #{ x : int }
[%%expect {|
Line 1, characters 0-41:
1 | type t : value mod dynamic = #{ x : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immediate
         because it is an unboxed record.
       But the kind of type "t" must be a subkind of value mod dynamic
         because of the annotation on the declaration of the type t.
|}]

(* Tuple payloads have no immutable-label contribution. *)
type t : value mod dynamic = A of int
[%%expect {|
Line 1, characters 0-37:
1 | type t : value mod dynamic = A of int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type definition does not satisfy its kind annotation value mod dynamic,
       because
       - boxed variants are not mod dynamic
       - int is not mod dynamic
|}]

type t : value mod dynamic = A of { x : int }
[%%expect {|
Line 1, characters 0-45:
1 | type t : value mod dynamic = A of { x : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of value mod dynamic
         because of the annotation on the declaration of the type t.
|}]

type t : value mod dynamic = A of { mutable x : int }
[%%expect {|
Line 1, characters 0-53:
1 | type t : value mod dynamic = A of { mutable x : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type definition does not satisfy its kind annotation value mod dynamic,
       because
       - boxed variants are not mod dynamic
       - mutable fields are not mod dynamic
|}]

type t : value mod contended = { mutable x : int; y : int }
[%%expect {|
Line 1, characters 0-59:
1 | type t : value mod contended = { mutable x : int; y : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type definition does not satisfy its kind annotation value mod contended,
       because mutable fields are not mod contended.
|}]

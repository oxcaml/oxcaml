(* TEST
   flags = "-no-ikinds";
   expect;
*)

(* Immutable record fields used to contribute an untracked [immediate] to the
   provenance residual, suppressing the error for the one bound it obstructs. *)

type t : value mod dynamic = { mutable x : int }
[%%expect {|
Line 1, characters 0-48:
1 | type t : value mod dynamic = { mutable x : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod dynamic
         because of the annotation on the declaration of the type t.
|}]

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

type t : value mod dynamic = A of int
[%%expect {|
Line 1, characters 0-37:
1 | type t : value mod dynamic = A of int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of value mod dynamic
         because of the annotation on the declaration of the type t.
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
Error: The kind of type "t" is mutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of value mod dynamic
         because of the annotation on the declaration of the type t.
|}]

type t : value mod contended = { mutable x : int; y : int }
[%%expect {|
Line 1, characters 0-59:
1 | type t : value mod contended = { mutable x : int; y : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod contended
         because of the annotation on the declaration of the type t.
|}]

(* Each base value has its own wording; these all pick [immediate]. *)

type t : value mod dynamic = { x : int } [@@unboxed]
[%%expect {|
Line 1, characters 0-52:
1 | type t : value mod dynamic = { x : int } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immediate
         because it is the primitive type int.
       But the kind of type "t" must be a subkind of value mod dynamic
         because of the annotation on the declaration of the type t.
|}]

type t : value mod dynamic = A of int [@@unboxed]
[%%expect {|
Line 1, characters 0-49:
1 | type t : value mod dynamic = A of int [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immediate
         because it is the primitive type int.
       But the kind of type "t" must be a subkind of value mod dynamic
         because of the annotation on the declaration of the type t.
|}]

type t : value mod dynamic = A | B
[%%expect {|
Line 1, characters 0-34:
1 | type t : value mod dynamic = A | B
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immediate
         because it's an enumeration variant type (all constructors are constant).
       But the kind of type "t" must be a subkind of value mod dynamic
         because of the annotation on the declaration of the type t.
|}]

type void_t : void
type t : value mod dynamic = A of void_t [@immediate_all_void_constructor]
[%%expect {|
type void_t : void
Line 2, characters 0-74:
2 | type t : value mod dynamic = A of void_t [@immediate_all_void_constructor]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immediate with void_t
         because it's an enumeration variant type (all constructors are constant).
       But the kind of type "t" must be a subkind of value mod dynamic
         because of the annotation on the declaration of the type t.
|}]

(* A manifest with a plain mod-bounds annotation still bypasses provenance:
   [narrow_to_manifest_jkind] routes it through [constrain_type_jkind]. *)

type t : value mod dynamic = int * int
[%%expect {|
Line 1, characters 0-38:
1 | type t : value mod dynamic = int * int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int * int" is immutable_data
         because it's a tuple type.
       But the kind of type "int * int" must be a subkind of value mod dynamic
         because of the definition of t at line 1, characters 0-38.
|}]

type t : value mod contended = int ref
[%%expect {|
Line 1, characters 0-38:
1 | type t : value mod contended = int ref
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int ref" is mutable_data.
       But the kind of type "int ref" must be a subkind of value mod contended
         because of the definition of t at line 1, characters 0-38.
|}]

(* A with-bounds annotation takes the ikind route. *)
type 'a t : immutable_data with 'a = 'a ref
[%%expect {|
Line 1, characters 0-43:
1 | type 'a t : immutable_data with 'a = 'a ref
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a ref" is
           mutable_data with 'a @@ forkable unyielding many.
       But the kind of type "'a ref" must be a subkind of
           immutable_data with 'a
         because of the definition of t at line 1, characters 0-43.

       The first mode-crosses less than the second along:
         contention: mod uncontended ≰ mod contended with 'a
         visibility: mod read_write ≰ mod immutable with 'a
|}]

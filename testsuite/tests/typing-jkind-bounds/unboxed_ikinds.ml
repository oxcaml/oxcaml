(* TEST
    expect;
*)

(*********************************************************)
(* When checking the annotation on an unboxed declaration, we shouldn't look
   through abstract kinds. *)

type 'a abs : immutable_data with 'a
type 'a t : immutable_data with 'a abs = { foo : 'a abs } [@@unboxed]
[%%expect {|
type 'a abs : immutable_data with 'a
Line 2, characters 0-69:
2 | type 'a t : immutable_data with 'a abs = { foo : 'a abs } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because of the definition of abs at line 1, characters 0-36.
       But the kind of type "t" must be a subkind of immutable_data with 'a abs
         because of the annotation on the declaration of the type t.
|}]

type 'a abs : immutable_data with 'a
type 'a t : immutable_data with 'a abs = Foo of 'a abs [@@unboxed]
[%%expect {|
type 'a abs : immutable_data with 'a
Line 2, characters 0-66:
2 | type 'a t : immutable_data with 'a abs = Foo of 'a abs [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because of the definition of abs at line 1, characters 0-36.
       But the kind of type "t" must be a subkind of immutable_data with 'a abs
         because of the annotation on the declaration of the type t.
|}]

type 'a abs : immutable_data with 'a
type 'a t : immutable_data with 'a abs = Foo of { foo : 'a abs } [@@unboxed]
[%%expect {|
type 'a abs : immutable_data with 'a
Line 2, characters 0-76:
2 | type 'a t : immutable_data with 'a abs = Foo of { foo : 'a abs } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because of the definition of abs at line 1, characters 0-36.
       But the kind of type "t" must be a subkind of immutable_data with 'a abs
         because of the annotation on the declaration of the type t.
|}]

(*********************************************************)
(* When checking the annotation on an unboxed declaration, modalities should be
   respected. *)

type 'a t : value mod portable = { foo : 'a @@ portable } [@@unboxed]
[%%expect {|
type 'a t = { foo : 'a @@ portable; } [@@unboxed]
|}]

type 'a t : value mod portable = Foo of 'a @@ portable [@@unboxed]
[%%expect {|
Line 1, characters 0-66:
1 | type 'a t : value mod portable = Foo of 'a @@ portable [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value
         because it instantiates an unannotated type parameter of t,
         chosen to have kind value.
       But the kind of type "t" must be a subkind of value mod portable
         because of the annotation on the declaration of the type t.
|}]

type 'a t : value mod portable = Foo of { foo : 'a @@ portable } [@@unboxed]
[%%expect {|
Line 1, characters 0-76:
1 | type 'a t : value mod portable = Foo of { foo : 'a @@ portable } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value
         because it instantiates an unannotated type parameter of t,
         chosen to have kind value.
       But the kind of type "t" must be a subkind of value mod portable
         because of the annotation on the declaration of the type t.
|}]

(*********************************************************)
(* Existentials in unboxed GADT payloads should be replaced with
   [(type : <<kind>>)] when computing the kind of the declaration. *)

type 'a abs : immutable_data with 'a
type t : immutable_data with (type : value) abs = Pack : 'a abs -> t [@@unboxed]
[%%expect {|
type 'a abs : immutable_data with 'a
Line 2, characters 0-80:
2 | type t : immutable_data with (type : value) abs = Pack : 'a abs -> t [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because of the definition of abs at line 1, characters 0-36.
       But the kind of type "t" must be a subkind of
           immutable_data with (type : value) abs
         because of the annotation on the declaration of the type t.
|}]

type 'a abs : immutable_data with 'a
type t : immutable_data with (type : value) abs =
  | Pack : { value : 'a abs } -> t [@@unboxed]
[%%expect {|
type 'a abs : immutable_data with 'a
Lines 2-3, characters 0-46:
2 | type t : immutable_data with (type : value) abs =
3 |   | Pack : { value : 'a abs } -> t [@@unboxed]
Error: The kind of type "t" is immutable_data with 'a
         because of the definition of abs at line 1, characters 0-36.
       But the kind of type "t" must be a subkind of
           immutable_data with (type : value) abs
         because of the annotation on the declaration of the type t.
|}]

(* Regression test for previously-existing bug *)
type ('a, 'k) data : value mod everything with 'a @@ contended portable
type 'k access : void mod everything
type ('a, 'k) unpacked
  : (value & void) mod everything with ('a, 'k) data with 'k access
  = #(('a, 'k) data * 'k access)
type 'a t
  : (value & void) mod everything with ('a, (type : value)) unpacked =
  | P : ('a, 'k) unpacked -> 'a t
[@@unboxed]
[%%expect {|
type ('a, 'k) data : value mod everything with 'a @@ portable contended
type 'k access : void mod everything
type ('a, 'k) unpacked = #(('a, 'k) data * 'k access)
Lines 6-9, characters 0-11:
6 | type 'a t
7 |   : (value & void) mod everything with ('a, (type : value)) unpacked =
8 |   | P : ('a, 'k) unpacked -> 'a t
9 | [@@unboxed]
Error: The kind of type "t" is
           value mod everything mod dynamic with 'k access with ('a, 'k) data
           & void mod everything
               mod dynamic
               with 'k access
               with ('a, 'k) data
         because it is an unboxed tuple.
       But the kind of type "t" must be a subkind of
           value mod everything
             with ('a, (type : value)) data
             with (type : value) access
           & void mod everything
               with ('a, (type : value)) data
               with (type : value) access
         because of the annotation on the declaration of the type t.
|}]

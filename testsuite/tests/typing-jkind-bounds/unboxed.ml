(* TEST
    flags = "-no-ikinds";
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
type 'a t = Foo of 'a abs [@@unboxed]
|}]

type 'a abs : immutable_data with 'a
type 'a t : immutable_data with 'a abs = Foo of { foo : 'a abs } [@@unboxed]
[%%expect {|
type 'a abs : immutable_data with 'a
type 'a t = Foo of { foo : 'a abs; } [@@unboxed]
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
type 'a t = Foo of 'a @@ portable [@@unboxed]
|}]

type 'a t : value mod portable = Foo of { foo : 'a @@ portable } [@@unboxed]
[%%expect {|
type 'a t = Foo of { foo : 'a @@ portable; } [@@unboxed]
|}]

(*********************************************************)
(* Existentials in unboxed GADT payloads should be replaced with
   [(type : <<kind>>)] when computing the kind of the declaration. *)

type 'a abs : immutable_data with 'a
type t : immutable_data with (type : value) abs = Pack : 'a abs -> t [@@unboxed]
[%%expect {|
type 'a abs : immutable_data with 'a
type t = Pack : 'a abs -> t [@@unboxed]
|}]

type 'a abs : immutable_data with 'a
type t : immutable_data with (type : value) abs =
  | Pack : { value : 'a abs } -> t [@@unboxed]
[%%expect {|
type 'a abs : immutable_data with 'a
type t = Pack : { value : 'a abs; } -> t [@@unboxed]
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
type 'a t = P : ('a, 'k) unpacked -> 'a t [@@unboxed]
|}]

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
type 'a t = { foo : 'a abs; } [@@unboxed]
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

(*********************************************************)
(* Local kind bounds within gadts should be respected. *)

type 'a t : immutable_data =
  | T : ('a : immutable_data). 'a -> 'a t
[@@unboxed]
[%%expect {|
Lines 1-3, characters 0-11:
1 | type 'a t : immutable_data =
2 |   | T : ('a : immutable_data). 'a -> 'a t
3 | [@@unboxed]
Error: The kind of type "t" is
           value mod everything non_float mod dynamic with 'a
         because it's an unboxed variant type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type 'a t : value mod portable =
  | T : ('a : value mod portable). { foo : 'a } -> 'a t
[@@unboxed]
[%%expect {|
Lines 1-3, characters 0-11:
1 | type 'a t : value mod portable =
2 |   | T : ('a : value mod portable). { foo : 'a } -> 'a t
3 | [@@unboxed]
Error: The kind of type "t" is value mod everything mod dynamic with 'a
         because it's an unboxed variant type.
       But the kind of type "t" must be a subkind of value mod portable
         because of the annotation on the declaration of the type t.
|}]

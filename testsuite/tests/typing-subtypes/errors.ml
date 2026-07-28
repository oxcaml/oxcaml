(* TEST
 flags = "-extension subtypes";
 expect;
*)

(* Prerequisite: a boxed variant type to use as a supertype. *)
type letter = A | B | C | D | E
[%%expect{|
type letter = A | B | C | D | E
|}]

(* An alias of a variant type cannot be a supertype (no alias chasing). *)
type l2 = letter
type w :> l2 = A
[%%expect{|
type l2 = letter
Line 2, characters 0-16:
2 | type w :> l2 = A
    ^^^^^^^^^^^^^^^^
Error: The supertype "l2" is not a variant type.
       Only a type declared as a variant can be a supertype.
|}]

(* An arrow type cannot be a supertype. *)
type w :> (int -> int) = A
[%%expect{|
Line 1, characters 0-26:
1 | type w :> (int -> int) = A
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The supertype "int -> int" is not a variant type.
       Only a type declared as a variant can be a supertype.
|}]

(* An extensible variant cannot be a supertype. *)
type ext = ..
type w :> ext = A
[%%expect{|
type ext = ..
Line 2, characters 0-17:
2 | type w :> ext = A
    ^^^^^^^^^^^^^^^^^
Error: The supertype "ext" is not a variant type.
       Only a type declared as a variant can be a supertype.
|}]

(* An extensible kind cannot declare a supertype. *)
type w2 :> letter = ..
[%%expect{|
Line 1, characters 0-22:
1 | type w2 :> letter = ..
    ^^^^^^^^^^^^^^^^^^^^^^
Error: Only variant types can declare a supertype.
|}]

(* A subtype cannot be [@@unboxed] (here the unboxed attribute itself is
   ill-formed, since the constructor is constant). *)
type u :> letter = A [@@unboxed]
[%%expect{|
Line 1, characters 0-32:
1 | type u :> letter = A [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because its constructor has no argument.
|}]

(* An [@@unboxed] variant cannot be a supertype: only boxed variants have
   the tags a subtype inherits. *)
type ubx = U of int [@@unboxed]
type w :> ubx = U of int
[%%expect{|
type ubx = U of int [@@unboxed]
Line 2, characters 0-24:
2 | type w :> ubx = U of int
    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The supertype "ubx" is not a variant type.
       Only a type declared as a variant can be a supertype.
|}]

(* Constructor argument types must equal the supertype's. *)
type args = X of int | Y of string
type w :> args = X of string
[%%expect{|
type args = X of int | Y of string
Line 2, characters 0-28:
2 | type w :> args = X of string
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "args"
       Constructors do not match:
         "X of int"
       is not the same as:
         "X of string"
       The type "int" is not equal to the type "string"
|}]

(* Constructor arity must equal the supertype's. *)
type w :> args = Y of string * string
[%%expect{|
Line 1, characters 0-37:
1 | type w :> args = Y of string * string
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "args"
       Constructors do not match:
         "Y of string"
       is not the same as:
         "Y of string * string"
       They have different arities.
|}]

(* Constructors must appear in the supertype's relative order. *)
type w :> letter = E | A
[%%expect{|
Line 1, characters 0-24:
1 | type w :> letter = E | A
    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "letter"
       1. Constructors have different names, "A" and "E".
       2. Constructors have different names, "B" and "A".
|}]

(* A tuple argument cannot match the supertype's inline record. *)
type ir = R of { x : int } | S
type w :> ir = R of int
[%%expect{|
type ir = R of { x : int; } | S
Line 2, characters 0-23:
2 | type w :> ir = R of int
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "ir"
       Constructors do not match:
         "R of { x : int; }"
       is not the same as:
         "R of int"
       The original uses inline records and this doesn't.
|}]

(* A mutable inline-record payload is fine when literally equal. *)
type ir2 = M of { mutable x : int } | N
type w :> ir2 = M of { mutable x : int }
[%%expect{|
type ir2 = M of { mutable x : int; } | N
type w :> ir2 = M of { mutable x : int; }
|}]

(* Recursive occurrences must match the supertype literally: [K of w]
   does not match [K of rt]. *)
type rt = K of rt | L
type w :> rt = K of w
[%%expect{|
type rt = K of rt | L
Line 2, characters 0-21:
2 | type w :> rt = K of w
    ^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "rt"
       Constructors do not match:
         "K of rt"
       is not the same as:
         "K of w"
       The type "rt" is not equal to the type "w"
|}]

(* ... but the literal form [K of rt] is accepted. *)
type w2 :> rt = K of rt
[%%expect{|
type w2 :> rt = K of rt
|}]

(* GADT syntax: the subtype constructor's result type is the subtype
   itself, which differs from the supertype's result type. *)
type g = G : int -> g
type w :> g = G : int -> w
[%%expect{|
type g = G : int -> g
Line 2, characters 0-26:
2 | type w :> g = G : int -> w
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "g"
       Constructors do not match:
         "G : int -> g"
       is not the same as:
         "G : int -> w"
       The type "g" is not equal to the type "w"
|}]

(* GADT result type naming the supertype: best guess is that the usual
   GADT return-type check rejects it (recording actual behavior). *)
type w2 :> g = G : int -> g
[%%expect{|
Line 1, characters 26-27:
1 | type w2 :> g = G : int -> g
                              ^
Error: Constraints are not satisfied in this type.
       Type "g" should be an instance of "w2"
|}]

(* A private variant supertype's constructors may not be re-exposed by a
   public subtype. *)
type priv = private P | Q
type w :> priv = P
[%%expect{|
type priv = private P | Q
Line 2, characters 0-18:
2 | type w :> priv = P
    ^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "priv"
       Private variant constructor(s) would be revealed.
|}]

(* A manifest, a supertype, and an explicit definition may be combined:
   both the manifest and the supertype are checked against the
   definition. *)
type vowel :> letter = A | E
type both :> letter = vowel = A | E
[%%expect{|
type vowel :> letter = A | E
type both :> letter = vowel = A | E
|}]

(* An [@@unboxed] subtype that drops a constructor must be rejected: its
   value is the bare payload, so the free coercion would reinterpret an
   immediate as a boxed block (segfault / type confusion otherwise). *)
type ubx_drop = P of int | Q
type w :> ubx_drop = P of int [@@unboxed]
[%%expect{|
type ubx_drop = P of int | Q
Line 2, characters 0-41:
2 | type w :> ubx_drop = P of int [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A variant type with a supertype must use the default (boxed)
       representation. A type with an unboxed or special representation
       cannot be a subtype.
|}]

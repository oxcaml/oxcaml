(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* Records and tuples have box kinds, so they can be hidden at box kinds. *)

module M : sig
  type t : value box
end = struct
  type t = { i : int }
end
[%%expect{|
module M : sig type t : value box end
|}]

module N : sig
  type t : (value & value) box
end = struct
  type t = { i : int; s : string }
end
[%%expect{|
module N : sig type t : (value & value) box end
|}]

(* Mixed records too *)
module F : sig
  type t : (float64 & value) box
end = struct
  type t = { f : float#; i : int }
end
[%%expect{|
module F : sig type t : (float64 & value) box end
|}]

(* The payload components carry the fields' scannable axes *)
module P : sig
  type t : (value non_pointer & value non_float) box
end = struct
  type t = { i : int; s : string }
end
[%%expect{|
module P : sig type t : (value non_pointer & value non_float) box end
|}]

(* ...honestly: a nullable field cannot be hidden at a non-null payload *)
module Bad : sig
  type t : (value & value) box
end = struct
  type t = { x : int or_null; s : string }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { x : int or_null; s : string }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { x : int or_null; s : string; } end
       is not included in
         sig type t : (value & value) box end
       Type declarations do not match:
         type t = { x : int or_null; s : string; }
       is not included in
         type t : (value & value) box
       The layout of the first is
           (value_or_null non_pointer & value non_float) box
         because of the definition of t at line 4, characters 2-42.
       But the layout of the first must be a sublayout of (value & value) box
         because of the definition of t at line 2, characters 2-30.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

module Ok : sig
  type t : (value_or_null & value) box
end = struct
  type t = { x : int or_null; s : string }
end
[%%expect{|
module Ok : sig type t : (value_or_null & value) box end
|}]

(* Records without unboxed versions don't get box kinds *)
type bad : (float64 & float64) box = { x : float; y : float }
[%%expect{|
Line 1, characters 0-61:
1 | type bad : (float64 & float64) box = { x : float; y : float }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "bad" is value non_float
         because it's a boxed record type.
       But the layout of type "bad" must be a sublayout of
           (float64 & float64) box
         because of the annotation on the declaration of the type bad.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

type bad : value box = { x : float } [@@unboxed]
[%%expect{|
Line 1, characters 0-48:
1 | type bad : value box = { x : float } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "bad" is float64 box
         because it is the primitive type float.
       But the layout of type "bad" must be a sublayout of value box
         because of the annotation on the declaration of the type bad.
|}]

(* Variants don't have unboxed versions, so they don't get box kinds *)
type bad : value box = A of int
[%%expect{|
Line 1, characters 0-31:
1 | type bad : value box = A of int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "bad" is value non_float
         because it's a boxed variant type.
       But the layout of type "bad" must be a sublayout of value box
         because of the annotation on the declaration of the type bad.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

(* Tuples *)
module T : sig
  type t : (value & value) box
end = struct
  type t = int * string
end
[%%expect{|
module T : sig type t : (value & value) box end
|}]

(* Tuple components carry one level of scannable axes *)
module TP : sig
  type t : (value non_pointer & value non_float) box
end = struct
  type t = int * string
end
[%%expect{|
module TP : sig type t : (value non_pointer & value non_float) box end
|}]

module TBad : sig
  type t : (value & value) box
end = struct
  type t = int or_null * string
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = int or_null * string
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = int or_null * string end
       is not included in
         sig type t : (value & value) box end
       Type declarations do not match:
         type t = int or_null * string
       is not included in
         type t : (value & value) box
       The layout of the first is (value_or_null & value non_float) box
         because it's a tuple type.
       But the layout of the first must be a sublayout of (value & value) box
         because of the definition of t at line 2, characters 2-30.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

module TOk : sig
  type t : (value_or_null & value) box
end = struct
  type t = int or_null * string
end
[%%expect{|
module TOk : sig type t : (value_or_null & value) box end
|}]

(* The unboxed version of a tuple alias is still the unboxed tuple *)
type t = int * string
type tu = t#
let f (x : tu) : #(int * string) = x
[%%expect{|
type t = int * string
type tu = t#
val f : tu -> #(int * string) = <fun>
|}]

(* A record of records prints as boxes of its unboxed payload's kind, with
   no redundant scannable axes; [mod immutable] records what an immutable
   record crosses beyond its box kind (which withholds visibility and
   contention: [t# box = t] could be mutable) *)
type t = { i : int }
type s = { t : t }
type ('a : value mod global) req_mg
type bad = s req_mg
[%%expect{|
type t = { i : int; }
type s = { t : t; }
type ('a : value mod global) req_mg
Line 4, characters 11-12:
4 | type bad = s req_mg
               ^
Error: This type "s" should be an instance of type "('a : value mod global)"
       The kind of s is immutable_data
         because of the definition of s at line 2, characters 0-18.
       But the kind of s must be a subkind of value mod global
         because of the definition of req_mg at line 3, characters 0-35.
|}]

(* A record with an atomic field has no unboxed version, so no box kind *)
type bad : value box = { mutable a : int [@atomic] }
[%%expect{|
Line 1, characters 0-52:
1 | type bad : value box = { mutable a : int [@atomic] }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "bad" is value non_float
         because it's a boxed record type.
       But the layout of type "bad" must be a sublayout of value box
         because of the annotation on the declaration of the type bad.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

(* A component's box structure is checked on demand *)
type r = { x : int }
module M : sig
  type t : (value non_pointer box & value non_pointer) box
end = struct
  type t = r * int
end
[%%expect{|
type r = { x : int; }
module M : sig type t : (value non_pointer box & value non_pointer) box end
|}]

(* ...and is below the flattened form the block stores *)
module M : sig
  type t : (value non_float & value non_pointer) box
end = struct
  type t = r * int
end
[%%expect{|
module M : sig type t : (value non_float & value non_pointer) box end
|}]

(* Nested tuples check against box kinds as deep as the kind asks *)
module M : sig
  type t : (value non_float & value non_pointer) box
end = struct
  type t = (int * int) * int
end
[%%expect{|
module M : sig type t : (value non_float & value non_pointer) box end
|}]

module M : sig
  type t : (value & (value & value) box) box
end = struct
  type t = int * (int * int)
end
[%%expect{|
module M : sig type t : (value & (value & value) box) box end
|}]

type t : (value & (value & value) box) box = int * (int * int)
[%%expect{|
type t = int * (int * int)
|}]

type ('a : (value & (value & value) box) box) deep_req
type ok = (int * (int * int)) deep_req
type ok = (int * (int * (int * int))) deep_req
[%%expect{|
type ('a : (value & (value & value) box) box) deep_req
type ok = (int * (int * int)) deep_req
type ok = (int * (int * (int * int))) deep_req
|}]

type bad = (int * (int or_null * int)) deep_req
[%%expect{|
Line 1, characters 12-37:
1 | type bad = (int * (int or_null * int)) deep_req
                ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type "int * (int or_null * int)" should be an instance of type
         "('a : (value & (value & value) box) box)"
       The layout of int * (int or_null * int) is any box non_float non_null
         because it's a tuple type.
       But the layout of int * (int or_null * int) must be a sublayout of
           (value & (value & value) box) box
         because of the definition of deep_req at line 1, characters 0-54.
|}]

(* Records and boxes are looked into the same way *)
module R : sig
  type t : (value & (value & value) box) box
end = struct
  type t = { a : int; b : int * int }
end
type r2 = { a : int; b : int * int }
type ok = r2 deep_req
[%%expect{|
module R : sig type t : (value & (value & value) box) box end
type r2 = { a : int; b : int * int; }
type ok = r2 deep_req
|}]

module R2 : sig
  type t : (value & (value & (value & value) box) box) box
end = struct
  type t = { a : int; b : int * (int * int) }
end
type r3 : (value & (value & (value & value) box) box) box =
  { a : int; b : int * (int * int) }
type ('a : (value & (value & (value & value) box) box) box) deep2_req
type ok = r3 deep2_req
[%%expect{|
module R2 : sig type t : (value & (value & (value & value) box) box) box end
type r3 = { a : int; b : int * (int * int); }
type ('a : (value & (value & (value & value) box) box) box) deep2_req
type ok = r3 deep2_req
|}]

module B : sig
  type t : ((value & value) box & value) box
end = struct
  type t = #((int * int) * int) box
end
[%%expect{|
module B : sig type t : ((value & value) box & value) box end
|}]

(* A component's kind alias expands to its layout *)
kind_ kb = bits8 box
type k : kb
module M : sig
  type t : (bits8 box & value non_pointer) box
end = struct
  type t = k * int
end
[%%expect{|
kind_ kb = bits8 box
type k : bits8 box
module M : sig type t : (bits8 box & value non_pointer) box end
|}]

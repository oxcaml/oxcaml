(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(* A single immutable [float#] field becomes [Record_float_block]: a tag-253
   single-float block — the same runtime shape as a boxed [float]. *)

type t = { f : float# }
[%%expect{|
type t = { f : float#; }
|}]

(* Such records are NOT [value non_float] because they ARE float blocks. *)

type t2 : value non_float = { f : float# }
[%%expect{|
Line 1, characters 0-42:
1 | type t2 : value non_float = { f : float# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t2" is value
         because it's a boxed record type.
       But the layout of type "t2" must be a sublayout of value non_float
         because of the annotation on the declaration of the type t2.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

(* A record with a single field of layout [any] could later be filled in with
   a [float#] and become a float block, so it cannot promise [non_float]. *)

type ('a : any) t3 : value non_float = { a : 'a }
[%%expect{|
Line 1, characters 0-49:
1 | type ('a : any) t3 : value non_float = { a : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t3" is value
         because it's a boxed record type.
       But the layout of type "t3" must be a sublayout of value non_float
         because of the annotation on the declaration of the type t3.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

(* Multi-field records with [any] fields remain [value non_float] (they can't
   become a single-field float block). *)

type ('a : value, 'b : value) t5 : value non_float = { a : 'a; b : 'b }
[%%expect{|
type ('a, 'b) t5 = { a : 'a; b : 'b; }
|}]

(* With [@@represent_as_float_array], a single [float#] field still uses
   [Record_ufloat] (tag-254 flat float-array), which IS [value non_float]. *)

type t6 : value non_float = { f : float# } [@@represent_as_float_array]
[%%expect{|
type t6 = { f : float#; }
|}]

(* Mutable single-[float#] records are also [Record_float_block] and so are
   NOT [value non_float]. *)

type t7 : value non_float = { mutable f : float# }
[%%expect{|
Line 1, characters 0-50:
1 | type t7 : value non_float = { mutable f : float# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t7" is value
         because it's a boxed record type.
       But the layout of type "t7" must be a sublayout of value non_float
         because of the annotation on the declaration of the type t7.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

(* But without the annotation, it works fine. *)

type t8 = { mutable f : float# }
[%%expect{|
type t8 = { mutable f : float#; }
|}]

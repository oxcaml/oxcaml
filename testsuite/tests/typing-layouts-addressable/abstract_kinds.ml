(* TEST
 flags = "-extension layouts_alpha -w -181";
 expect;
*)

(* The [addressable] kind operator on abstract kind constructors. *)

type ('a : any addressable) require_addressable
[%%expect{|
type ('a : any addressable) require_addressable
|}]

(* [addressable] on an abstract kind constructor. *)

kind_ k
type t : k addressable
type check = t require_addressable
[%%expect{|
kind_ k
type t : k addressable
type check = t require_addressable
|}]

(* [k] and [k addressable] are incomparable while [k] is abstract. *)

kind_ ka
module M : sig
  type t : ka
end = struct
  type t : ka addressable
end
[%%expect{|
kind_ ka
Lines 4-6, characters 6-3:
4 | ......struct
5 |   type t : ka addressable
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : ka addressable end
       is not included in
         sig type t : ka end
       Type declarations do not match:
         type t : ka addressable
       is not included in
         type t : ka
       The kind of the first is ka addressable
         because of the definition of t at line 5, characters 2-25.
       But the kind of the first must be a subkind of ka
         because of the definition of t at line 3, characters 2-13.
|}]

module M : sig
  type t : ka addressable
end = struct
  type t : ka
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : ka
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : ka end
       is not included in
         sig type t : ka addressable end
       Type declarations do not match:
         type t : ka
       is not included in
         type t : ka addressable
       The kind of the first is ka
         because of the definition of t at line 4, characters 2-13.
       But the kind of the first must be a subkind of ka addressable
         because of the definition of t at line 2, characters 2-25.
|}]

(* Two applications to the same abstract kind agree. *)

module M : sig
  type t : ka addressable
end = struct
  type t : ka addressable
end
[%%expect{|
module M : sig type t : ka addressable end
|}]

(* [addressable] on a kind constructor with an addressable manifest is the
   identity. *)

kind_ kv = value
type tv : kv addressable
type checkv = tv require_addressable
[%%expect{|
kind_ kv = value
Line 2, characters 13-24:
2 | type tv : kv addressable
                 ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "kv".

type tv
type checkv = tv require_addressable
|}]

module M : sig
  type t : kv
end = struct
  type t : kv addressable
end
[%%expect{|
Line 4, characters 14-25:
4 |   type t : kv addressable
                  ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "kv".

module M : sig type t end
|}]

(* [addressable] on a kind constructor with an unaddressable manifest is
   the manifest made addressable... *)

kind_ k8 = bits8
type t8 : k8 addressable
type check8 = t8 require_addressable
[%%expect{|
kind_ k8 = bits8
type t8 : bits8 addressable
type check8 = t8 require_addressable
|}]

module M : sig
  type t : k8 addressable
end = struct
  type t : bits8 addressable
end
[%%expect{|
module M : sig type t : bits8 addressable end
|}]

(* ...and is distinct from the manifest. *)

module M : sig
  type t : k8
end = struct
  type t : k8 addressable
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : k8 addressable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : bits8 addressable end
       is not included in
         sig type t : bits8 end
       Type declarations do not match:
         type t : bits8 addressable
       is not included in
         type t : bits8
       The layout of the first is bits8 addressable
         because of the definition of t at line 4, characters 2-25.
       But the layout of the first must be a sublayout of bits8
         because of the definition of t at line 2, characters 2-13.
|}]

module M : sig
  type t : k8 addressable
end = struct
  type t : k8
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : k8
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : bits8 end
       is not included in
         sig type t : bits8 addressable end
       Type declarations do not match:
         type t : bits8
       is not included in
         type t : bits8 addressable
       The layout of the first is bits8
         because of the definition of t at line 4, characters 2-13.
       But the layout of the first must be a sublayout of bits8 addressable
         because of the definition of t at line 2, characters 2-25.
|}]

(* An abstract kind made addressable is below [any addressable] but the
   abstract kind itself is not. *)

kind_ kb
type tb : kb
type bad = tb require_addressable
[%%expect{|
kind_ kb
type tb : kb
Line 3, characters 11-13:
3 | type bad = tb require_addressable
               ^^
Error: This type "tb" should be an instance of type "('a : any addressable)"
       The kind of tb is kb
         because of the definition of tb at line 2, characters 0-12.
       But the kind of tb must be a subkind of any addressable
         because of the definition of require_addressable at line 1, characters 0-47.
|}]

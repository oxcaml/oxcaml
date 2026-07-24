(* TEST
 flags = "-extension layouts_alpha -w -181";
 expect;
*)

(* The [addressable] kind operator on abstract kind constructors. *)

type ('a : any addressable) require_addressable
[%%expect{|
Line 1, characters 15-26:
1 | type ('a : any addressable) require_addressable
                   ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* [addressable] on an abstract kind constructor. *)

kind_ k
type t : k addressable
type check = t require_addressable
[%%expect{|
kind_ k
Line 2, characters 11-22:
2 | type t : k addressable
               ^^^^^^^^^^^
Error: Unknown kind modifier addressable
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
Line 5, characters 14-25:
5 |   type t : ka addressable
                  ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

module M : sig
  type t : ka addressable
end = struct
  type t : ka
end
[%%expect{|
Line 2, characters 14-25:
2 |   type t : ka addressable
                  ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* Two applications to the same abstract kind agree. *)

module M : sig
  type t : ka addressable
end = struct
  type t : ka addressable
end
[%%expect{|
Line 4, characters 14-25:
4 |   type t : ka addressable
                  ^^^^^^^^^^^
Error: Unknown kind modifier addressable
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
Error: Unknown kind modifier addressable
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
Error: Unknown kind modifier addressable
|}]

(* [addressable] on a kind constructor with an unaddressable manifest is
   the manifest made addressable... *)

kind_ k8 = bits8
type t8 : k8 addressable
type check8 = t8 require_addressable
[%%expect{|
kind_ k8 = bits8
Line 2, characters 13-24:
2 | type t8 : k8 addressable
                 ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

module M : sig
  type t : k8 addressable
end = struct
  type t : bits8 addressable
end
[%%expect{|
Line 4, characters 17-28:
4 |   type t : bits8 addressable
                     ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* ...and is distinct from the manifest. *)

module M : sig
  type t : k8
end = struct
  type t : k8 addressable
end
[%%expect{|
Line 4, characters 14-25:
4 |   type t : k8 addressable
                  ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

module M : sig
  type t : k8 addressable
end = struct
  type t : k8
end
[%%expect{|
Line 2, characters 14-25:
2 |   type t : k8 addressable
                  ^^^^^^^^^^^
Error: Unknown kind modifier addressable
|}]

(* An abstract kind made addressable is below [any addressable] but the
   abstract kind itself is not. *)

kind_ kb
type tb : kb
type bad = tb require_addressable
[%%expect{|
kind_ kb
type tb : kb
Line 3, characters 14-33:
3 | type bad = tb require_addressable
                  ^^^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "require_addressable"
|}]

(* TEST
 flags = "-extension layouts_alpha -no-ikinds";
 expect;
*)

(* Tests for the [addressable] kind operator applied to abstract kinds. The
   operator cannot be pushed into an unexpanded kind path, so it is recorded
   on the kind constructor and applied when the path is expanded or
   substituted. This file is duplicated as [abstract_kinds_ikinds.ml], without
   [-no-ikinds]. *)

kind_ k

type t : k addressable
[%%expect{|
kind_ k
type t : k addressable
|}]

(* The operator is idempotent, so a second application is redundant *)
type t2 : k addressable addressable
[%%expect{|
Line 1, characters 24-35:
1 | type t2 : k addressable addressable
                            ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "k addressable".

type t2 : k addressable
|}]

(* [k addressable <= any addressable] for any abstract [k]... *)
type ('a : any addressable) req

type ok = t req
[%%expect{|
type ('a : any addressable) req
type ok = t req
|}]

(* ...but a plain [k] is of unknown addressability *)
type tk : k

type bad = tk req
[%%expect{|
type tk : k
Line 3, characters 11-13:
3 | type bad = tk req
               ^^
Error: This type "tk" should be an instance of type "('a : any addressable)"
       The kind of tk is k
         because of the definition of tk at line 1, characters 0-11.
       But the kind of tk must be a subkind of any addressable
         because of the definition of req at line 1, characters 0-31.
|}]

(* Refining an annotated type parameter computes the intersection of
   [k addressable] with the bound [any addressable] *)
type ('a : k addressable) refined = 'a req
[%%expect{|
type ('a : k addressable) refined = 'a req
|}]

type ('a : k) not_refined = 'a req
[%%expect{|
Line 1, characters 28-30:
1 | type ('a : k) not_refined = 'a req
                                ^^
Error: This type "('a : k)" should be an instance of type
         "('b : any addressable)"
       The layout of 'a is the abstract kind k
         because of the annotation on 'a in the declaration of the type
                                      not_refined.
       But the layout of 'a must overlap with any addressable
         because of the definition of req at line 1, characters 0-31.
|}]

(* [k addressable] and [k] are incomparable for an abstract [k] *)
module M : sig
  type t : k addressable
end = struct
  type t : k
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : k
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : k end
       is not included in
         sig type t : k addressable end
       Type declarations do not match:
         type t : k
       is not included in
         type t : k addressable
       The kind of the first is k
         because of the definition of t at line 4, characters 2-12.
       But the kind of the first must be a subkind of k addressable
         because of the definition of t at line 2, characters 2-24.
|}]

module M : sig
  type t : k
end = struct
  type t : k addressable
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : k addressable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : k addressable end
       is not included in
         sig type t : k end
       Type declarations do not match:
         type t : k addressable
       is not included in
         type t : k
       The kind of the first is k addressable
         because of the definition of t at line 4, characters 2-24.
       But the kind of the first must be a subkind of k
         because of the definition of t at line 2, characters 2-12.
|}]

module M : sig
  type t : k addressable
end = struct
  type t : k addressable
end
[%%expect{|
module M : sig type t : k addressable end
|}]

(* Kind aliases of made-addressable abstract kinds *)
kind_ ka = k addressable

type t' : ka

module M : sig
  type t : k addressable
end = struct
  type t : ka
end
[%%expect{|
kind_ ka = k addressable
type t' : k addressable
module M : sig type t : k addressable end
|}]

(* The pending operator is applied when the kind is substituted. [bits64] is
   addressable, so [k addressable] becomes just [bits64]... *)
module type S = sig
  kind_ k

  type t : k addressable

  val mk : unit -> t
end

module M64 : S with kind_ k = bits64 = struct
  kind_ k = bits64

  type t = int64#

  let mk () = #0L
end

let use () = M64.mk ()
[%%expect{|
module type S = sig kind_ k type t : k addressable val mk : unit -> t end
module M64 : sig kind_ k = bits64 type t : bits64 val mk : unit -> t end
val use : unit -> M64.t = <fun>
|}]

(* ...whereas [bits8] is not, so [k addressable] becomes [bits8 addressable],
   which [int8#]'s kind [bits8] does not satisfy *)
module Bad : S with kind_ k = bits8 = struct
  kind_ k = bits8

  type t = int8#

  let mk () = assert false
end
[%%expect{|
Lines 1-7, characters 38-3:
1 | ......................................struct
2 |   kind_ k = bits8
3 |
4 |   type t = int8#
5 |
6 |   let mk () = assert false
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k = bits8 type t = int8# val mk : unit -> 'a end
       is not included in
         sig
           kind_ k = bits8
           type t : bits8 addressable
           val mk : unit -> t
         end
       Type declarations do not match:
         type t = int8#
       is not included in
         type t : bits8 addressable
       The layout of the first is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of the first must be a sublayout of bits8 addressable
         because of the definition of t at line 4, characters 2-24.
|}]

module type T = sig
  kind_ k

  type t : k addressable
end

module Ok8 : T with kind_ k = bits8 = struct
  kind_ k = bits8

  type t : bits8 addressable
end
[%%expect{|
module type T = sig kind_ k type t : k addressable end
module Ok8 : sig kind_ k = bits8 type t : bits8 addressable end
|}]

(* The pending operator is also applied when the kind alias is expanded *)
kind_ kv = value

type t : kv addressable [@@warning "-183"]

let f (x : t) (y : t) = x == y
[%%expect{|
kind_ kv = value
type t
val f : t -> t -> bool = <fun>
|}]

kind_ k8 = bits8

type t8 : k8 addressable

let f (x : t8) = x
[%%expect{|
kind_ k8 = bits8
type t8 : bits8 addressable
val f : t8 -> t8 = <fun>
|}]

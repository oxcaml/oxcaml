(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* [@@unboxed] matches *)

type a : any
type t = { t : a } [@@unboxed]
let f { t } = t
[%%expect{|
type a : any
type t = { t : a; } [@@unboxed]
Line 3, characters 6-11:
3 | let f { t } = t
          ^^^^^
Error: This expression has type "('a : '_representable_layout_1)"
       but an expression was expected of type "t"
       The layout of t is any
         because of the definition of a at line 1, characters 0-12.
       But the layout of t must be representable
         because it's the record type used in a projection.
|}]

type ('a : any) t = { t : 'a } [@@unboxed]
let f { t } = t
[%%expect{|
type ('a : any) t = { t : 'a; } [@@unboxed]
val f : 'a t -> 'a = <fun>
|}]

(* Sort variables are not initialized well *)

(* Projecting a label other than the [any] *)

type ('a : any) t = { i : int ; a : 'a }
let foo t = (.a)
[%%expect{|
type ('a : any) t = { i : int; a : 'a; }
Line 2, characters 14-15:
2 | let foo t = (.a)
                  ^
Error: Cannot access record with unrepresentable field.
       The record has type 'a t, whose field a is not representable.
|}]

(* Matching *)

type ('a : any) t = { a : 'a }
let foo { a } = ()
[%%expect{|
type ('a : any) t = { a : 'a; }
Line 2, characters 8-13:
2 | let foo { a } = ()
            ^^^^^
Error: Cannot access record with unrepresentable field.
       The record has type 'a t, whose field a is not representable.
|}]

(* Block indices *)

(* CR-soon rtjoa: This should typecheck and default to value *)
type ('a : any) r = { t : 'a }
let f = (.t)
[%%expect{|
type ('a : any) r = { t : 'a; }
Line 2, characters 10-11:
2 | let f = (.t)
              ^
Error: Cannot access record with unrepresentable field.
       The record has type 'a r, whose field t is not representable.
|}]

(* Abstract kinds *)

kind_ k
type a : k
type t = { a : a }
let f { a } = ()
[%%expect{|
kind_ k
type a : k
type t = { a : a; }
Line 4, characters 6-11:
4 | let f { a } = ()
          ^^^^^
Error: Cannot access record with unrepresentable field.
       The record has type t, whose field a is not representable.
|}]

module M : sig
  kind_ k
  type t : k
  type a = A of t
end with kind_ k := void and type t := unit# = struct
  type a = A of unit#
end
[%%expect{|
Lines 5-7, characters 47-3:
5 | ...............................................struct
6 |   type a = A of unit#
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a = A of unit# end
       is not included in
         sig type a = A of unit# end
       Type declarations do not match:
         type a = A of unit#
       is not included in
         type a = A of unit#
       Constructors do not match:
         "A of unit#"
       is not the same as:
         "A of unit#"
       The first has a fixed representation and the second doesn't.
       Hint: Is there a type that has a representable layout in the first
         but has layout any in the second?
|}]

(* TEST
 flags = "-extension layout_poly_alpha";
 expect;
*)

(* Interaction of the [addressable] kind operator with lpoly layout
   variables ([layout_ x. ...]), which are rigid stand-ins for an unknown
   layout. For a rigid [x], the kinds [x] and [x addressable] must be
   treated as incomparable (like an abstract kind [k] vs [k addressable]),
   because [x] may be instantiated with an unaddressable layout, where they
   are distinct kinds. *)

(* The operator can be applied to a layout variable. *)
module type S = sig
  val f : layout_ x. ('a : x) ('b : x addressable). 'a -> 'b
end
[%%expect{|
module type S = sig val f : layout_ l. ('a : l) ('b : l). 'a -> 'b end
|}]

(* [x <= x addressable] must not hold. *)
module F (M : sig val f : layout_ x. ('a : x addressable). 'a -> 'a end) : sig
  val f : layout_ x. ('a : x). 'a -> 'a
end = M
[%%expect{|
Line 3, characters 6-7:
3 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : layout_ l. ('a : l). 'a -> 'a end
       is not included in
         sig val f : layout_ l. ('a : l). 'a -> 'a end
       Values do not match:
         val f : layout_ l. ('a : l). 'a -> 'a
       is not included in
         val f : layout_ l. ('a : l). 'a -> 'a
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The layout of 'a is '_representable_layout_1
         because of the definition of f at line 2, characters 2-39.
       But the layout of 'a must be a sublayout of
           '_representable_layout_1 addressable
         because of the definition of f at line 1, characters 18-67.
|}]

(* [x addressable <= x] must not hold either: [x] is rigid, so nothing
   justifies using a type of kind [x addressable] at kind [x]. *)
module G (M : sig val f : layout_ x. ('a : x). 'a -> 'a end) : sig
  val f : layout_ x. ('a : x addressable). 'a -> 'a
end = M
[%%expect{|
Line 3, characters 6-7:
3 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : layout_ l. ('a : l). 'a -> 'a end
       is not included in
         sig val f : layout_ l. ('a : l). 'a -> 'a end
       Values do not match:
         val f : layout_ l. ('a : l). 'a -> 'a
       is not included in
         val f : layout_ l. ('a : l). 'a -> 'a
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The layout of 'a is '_representable_layout_2 addressable
         because of the definition of f at line 2, characters 2-51.
       But the layout of 'a must be a sublayout of '_representable_layout_2
         because of the definition of f at line 1, characters 18-55.
|}]

(* A GADT match on an equality between kinds [x] and [x addressable] must
   NOT be refutable: at addressable instantiations of [x] (e.g. [bits64])
   the two kinds coincide, so [Refl] is possible. *)
type ('a : any, 'b : any) eq = Refl : ('a : any). ('a, 'a) eq

let no_refute : layout_ x. ('a : x) ('b : x addressable) 'r. ('a, 'b) eq -> 'r
  = fun x -> match x with _ -> .
[%%expect{|
type ('a : any, 'b : any) eq = Refl : ('a : any). ('a, 'a) eq
Line 3, characters 16-78:
3 | let no_refute : layout_ x. ('a : x) ('b : x addressable) 'r. ('a, 'b) eq -> 'r
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Layout polymorphism is not supported in term-level type annotations
|}]

(* TEST
 flags = "-extension layout_poly_alpha";
 expect;
*)

(* Sort-polymorphic value declarations in module types *)
module type S = sig
  val foo : layout_ x y. ('a : x) ('b : y). 'a -> 'b
end
[%%expect{|
module type S = sig val foo : layout_ x y. ('a : x) ('b : y). 'a -> 'b end
|}]

(* unused layout variables are not omitted *)
module type S = sig
  val foo : layout_ x. ('a : value) ('b : value). 'a -> 'b
end
[%%expect{|
Line 2, characters 20-21:
2 |   val foo : layout_ x. ('a : value) ('b : value). 'a -> 'b
                        ^
Warning 191 [unused-kind-declaration]: unused kind x.

module type S = sig val foo : layout_ x. 'a -> 'b end
|}]

(* shadowing of variable names is allowed; the printing back is smart enough tho *)
module type S = sig
  val bar : layout_ x x. ('a : x) ('b : x). 'a -> 'b
  val baz : layout_ x x x. ('a : x) ('b : x). 'a -> 'b
end
[%%expect{|
Line 2, characters 20-21:
2 |   val bar : layout_ x x. ('a : x) ('b : x). 'a -> 'b
                        ^
Warning 191 [unused-kind-declaration]: unused kind x.

Line 3, characters 20-21:
3 |   val baz : layout_ x x x. ('a : x) ('b : x). 'a -> 'b
                        ^
Warning 191 [unused-kind-declaration]: unused kind x.

Line 3, characters 22-23:
3 |   val baz : layout_ x x x. ('a : x) ('b : x). 'a -> 'b
                          ^
Warning 191 [unused-kind-declaration]: unused kind x.

module type S =
  sig
    val bar : layout_ x x0. ('a : x0) ('b : x0). 'a -> 'b
    val baz : layout_ x x0 x1. ('a : x1) ('b : x1). 'a -> 'b
  end
|}]

(* the layout variables are rigid and cannot be constrained *)
module type T = sig
  val bar : layout_ x y. ('a : x) ('b : y). ('a * 'b) -> unit
end
[%%expect{|
Line 2, characters 45-47:
2 |   val bar : layout_ x y. ('a : x) ('b : y). ('a * 'b) -> unit
                                                 ^^
Error: Tuple element types must have layout value.
       The layout of "'a" is the abstract kind x
         because of the annotation on the universal variable 'a.
       But the layout of "'a" must overlap with value
         because it's the type of a tuple element.
|}]

(* CR-someday zqian: some of the following inclusion check might succeed in the future
once we support coercion *)

(* implementation has more variables than the interface *)
module F2 (M : sig
  val f : layout_ x y. ('a : x). 'a -> 'a
end) : sig
  val f : layout_ x. ('a : x). 'a -> 'a
end = M
[%%expect{|
Line 5, characters 6-7:
5 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : layout_ x y. ('a : x). 'a -> 'a end
       is not included in
         sig val f : layout_ x. ('a : x). 'a -> 'a end
       Values do not match:
         val f : layout_ x y. ('a : x). 'a -> 'a
       is not included in
         val f : layout_ x. ('a : x). 'a -> 'a
       The number of locally abstract layouts differs.
|}]

(* implementation has fewer variables than the interface *)
module F2 (M : sig
  val f : layout_ x. ('a : x). 'a -> 'a
end) : sig
  val f : layout_ x y. ('a : x). 'a -> 'a
end = M
[%%expect{|
Line 5, characters 6-7:
5 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : layout_ x. ('a : x). 'a -> 'a end
       is not included in
         sig val f : layout_ x y. ('a : x). 'a -> 'a end
       Values do not match:
         val f : layout_ x. ('a : x). 'a -> 'a
       is not included in
         val f : layout_ x y. ('a : x). 'a -> 'a
       The number of locally abstract layouts differs.
|}]

(* same arity, but used different variable *)
module F2 (M : sig
  val f : layout_ x y. ('a : x). 'a -> 'a
end) : sig
  val f : layout_ x y. ('b : y). 'b -> 'b
end = M
[%%expect{|
Line 5, characters 6-7:
5 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : layout_ x y. ('a : x). 'a -> 'a end
       is not included in
         sig val f : layout_ x y. ('b : y). 'b -> 'b end
       Values do not match:
         val f : layout_ x y. ('a : x). 'a -> 'a
       is not included in
         val f : layout_ x y. ('b : y). 'b -> 'b
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The layout of 'a is y
         because of the definition of f at line 4, characters 2-41.
       But the layout of 'a must be a sublayout of x
         because of the definition of f at line 2, characters 2-41.
|}]

(* CR-someday zqian: the error message is confusing. *)
module F2 (M : sig
  val f : layout_ x y. ('a : x). 'a -> 'a
end) : sig
  val f : layout_ y x. ('b : x). 'b -> 'b
end = M
[%%expect{|
Line 5, characters 6-7:
5 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : layout_ x y. ('a : x). 'a -> 'a end
       is not included in
         sig val f : layout_ y x. ('b : x). 'b -> 'b end
       Values do not match:
         val f : layout_ x y. ('a : x). 'a -> 'a
       is not included in
         val f : layout_ y x. ('b : x). 'b -> 'b
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The layout of 'a is x
         because of the definition of f at line 4, characters 2-41.
       But the layout of 'a must be a sublayout of x
         because of the definition of f at line 2, characters 2-41.
|}]

(* some alpha renaming *)
module F1 (M : sig
  val f : layout_ x y. ('a : x) ('b : y). 'a -> 'b
end) : sig
  val f : layout_ p q. ('a : p) ('b : q). 'a -> 'b
end = M
[%%expect{|
module F1 :
  functor (M : sig val f : layout_ x y. ('a : x) ('b : y). 'a -> 'b end) ->
    sig val f : layout_ p q. ('a : p) ('b : q). 'a -> 'b end
|}]

(* layout-poly is not included in non-poly functions, even tho the former can be instantiate to the latter. *)
module F3 (M : sig
  val f : layout_ x. ('a : x). 'a -> 'a
end) : sig
  val f : 'a -> 'a
end = M
[%%expect{|
Line 5, characters 6-7:
5 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : layout_ x. ('a : x). 'a -> 'a end
       is not included in
         sig val f : 'a -> 'a end
       Values do not match:
         val f : layout_ x. ('a : x). 'a -> 'a
       is not included in
         val f : 'a -> 'a
       The number of locally abstract layouts differs.
|}]

(* Ordering: both use first var on both sides - same position, should succeed *)
module FO1 (M : sig
  val f : layout_ x y. ('a : x) ('b : x). 'a -> 'b
end) : sig
  val f : layout_ p q. ('a : p) ('b : p). 'a -> 'b
end = M
[%%expect{|
Line 2, characters 20-21:
2 |   val f : layout_ x y. ('a : x) ('b : x). 'a -> 'b
                        ^
Warning 191 [unused-kind-declaration]: unused kind y.

Line 4, characters 20-21:
4 |   val f : layout_ p q. ('a : p) ('b : p). 'a -> 'b
                        ^
Warning 191 [unused-kind-declaration]: unused kind q.

module FO1 :
  functor (M : sig val f : layout_ x y. ('a : x) ('b : x). 'a -> 'b end) ->
    sig val f : layout_ p q. ('a : p) ('b : p). 'a -> 'b end
|}]

(* Ordering: sort var in the same order, type var in different order, accepted. *)
module FO3(M : sig
  val f : layout_ p q. ('a : p) ('b : q). 'a -> 'b
end) : sig
  val f : layout_ p' q'. ('b : q') ('a : p'). 'a -> 'b
end = M
[%%expect{|
module FO3 :
  functor (M : sig val f : layout_ p q. ('a : p) ('b : q). 'a -> 'b end) ->
    sig val f : layout_ p' q'. ('a : p') ('b : q'). 'a -> 'b end
|}]

(* Ordering: sorts swapped between sides - should fail *)
module FO3 (M : sig
  val f : layout_ x y. ('a : x) ('b : y). 'a -> 'b
end) :  sig
  val f : layout_ p q. ('a : q) ('b : p). 'a -> 'b
end = M
[%%expect{|
Line 5, characters 6-7:
5 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : layout_ x y. ('a : x) ('b : y). 'a -> 'b end
       is not included in
         sig val f : layout_ p q. ('a : q) ('b : p). 'a -> 'b end
       Values do not match:
         val f : layout_ x y. ('a : x) ('b : y). 'a -> 'b
       is not included in
         val f : layout_ p q. ('a : q) ('b : p). 'a -> 'b
       The type "'a -> 'b" is not compatible with the type "'c -> 'd"
       The layout of 'a is q
         because of the definition of f at line 4, characters 2-50.
       But the layout of 'a must be a sublayout of x
         because of the definition of f at line 2, characters 2-50.
|}]

(* layout_ in a general type annotation is not yet supported *)
let _ = (fun (x : layout_ a. ('t : a). 't) -> x)
[%%expect{|
Line 1, characters 18-41:
1 | let _ = (fun (x : layout_ a. ('t : a). 't) -> x)
                      ^^^^^^^^^^^^^^^^^^^^^^^
Error: Sort polymorphism is not supported in this context
|}]

let f : layout_ a. ('t : a). 't -> 't = fun x -> x
[%%expect{|
Line 1, characters 8-37:
1 | let f : layout_ a. ('t : a). 't -> 't = fun x -> x
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Sort polymorphism is not supported in this context
|}]

(* layout_ in a record field type is not yet supported *)
type t = { id : layout_ k. ('a : k). 'a -> 'a }
[%%expect{|
Line 1, characters 16-45:
1 | type t = { id : layout_ k. ('a : k). 'a -> 'a }
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Sort polymorphism is not supported in this context
|}]

module F (M : sig val f : layout_ x. ('a : x). 'a -> 'a end) = struct
  let () = let _ = M.f in ()
end
[%%expect{|
Line 2, characters 19-22:
2 |   let () = let _ = M.f in ()
                       ^^^
Error: Instantiation of layout-polymorphic values is not yet supported.
|}]

(* You can add additional constraint on the modal bounds, which doesn't affect
   the layouts *)
module type T = sig
  val bar : layout_ x. ('a : x mod contended) ('b : x). 'a -> 'b
end
[%%expect{|
module type T =
  sig val bar : layout_ x. ('a : x mod contended) ('b : x). 'a -> 'b end
|}]

module F (M : sig
  val bar : layout_ x. ('a : x mod contended) ('b : x). 'a -> 'b
end) : sig
  val bar : layout_ x. ('a : x mod contended) ('b : x mod contended). 'a -> 'b
end = M
[%%expect{|
module F :
  functor
    (M : sig
           val bar : layout_ x. ('a : x mod contended) ('b : x). 'a -> 'b
         end)
    ->
    sig
      val bar :
        layout_ x. ('a : x mod contended) ('b : x mod contended). 'a -> 'b
    end
|}]

(* CR zqian: the error message should mention modal bounds; this is because (it
seems like) jkind error reporting happens outside of the inclusion check (where
the abstract univar are marked equal). *)
module F (M : sig
  val bar : layout_ x. ('a : x mod contended) ('b : x). 'a -> 'b
end) : sig
  val bar : layout_ x. ('a : x) ('b : x). 'a -> 'b
end = M
[%%expect{|
Line 5, characters 6-7:
5 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig
           val bar : layout_ x. ('a : x mod contended) ('b : x). 'a -> 'b
         end
       is not included in
         sig val bar : layout_ x. ('a : x) ('b : x). 'a -> 'b end
       Values do not match:
         val bar : layout_ x. ('a : x mod contended) ('b : x). 'a -> 'b
       is not included in
         val bar : layout_ x. ('a : x) ('b : x). 'a -> 'b
       The type "'a -> 'b" is not compatible with the type "'c -> 'd"
       The layout of 'a is x
         because of the definition of bar at line 4, characters 2-50.
       But the layout of 'a must be a sublayout of x
         because of the definition of bar at line 2, characters 2-64.
|}]


module F (M : sig
  val bar : layout_ x. ('a : x) ('b : x). 'a -> 'b
end) : sig
  val bar : layout_ x. ('a : x mod contended) ('b : x). 'a -> 'b
end = M
[%%expect{|
module F :
  functor (M : sig val bar : layout_ x. ('a : x) ('b : x). 'a -> 'b end) ->
    sig val bar : layout_ x. ('a : x mod contended) ('b : x). 'a -> 'b end
|}]

module F (M : sig
  val bar : layout_ x. ('a : x mod contended) ('b : x mod contended). 'a -> 'b
end) : sig
  val bar : layout_ x. ('a : x mod contended) ('b : x). 'a -> 'b
end = M
[%%expect{|
Line 5, characters 6-7:
5 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig
           val bar :
             layout_ x.
               ('a : x mod contended) ('b : x mod contended). 'a -> 'b
         end
       is not included in
         sig
           val bar : layout_ x. ('a : x mod contended) ('b : x). 'a -> 'b
         end
       Values do not match:
         val bar :
           layout_ x. ('a : x mod contended) ('b : x mod contended). 'a -> 'b
       is not included in
         val bar : layout_ x. ('a : x mod contended) ('b : x). 'a -> 'b
       The type "'a -> 'b" is not compatible with the type "'a -> 'c"
       The layout of 'a is x
         because of the definition of bar at line 4, characters 2-64.
       But the layout of 'a must be a sublayout of x
         because of the definition of bar at line 2, characters 2-78.
|}]

module F (M : sig
  val bar : layout_ x. ('a : x mod contended) ('b : x). 'a -> 'b
end) : sig
  val bar : layout_ x. ('a : x mod contended) ('b : x mod contended). 'a -> 'b
end = M
[%%expect{|
module F :
  functor
    (M : sig
           val bar : layout_ x. ('a : x mod contended) ('b : x). 'a -> 'b
         end)
    ->
    sig
      val bar :
        layout_ x. ('a : x mod contended) ('b : x mod contended). 'a -> 'b
    end
|}]

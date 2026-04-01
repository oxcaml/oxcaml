(* TEST
 flags = "-extension layout_poly_alpha";
 expect;
*)

(* Sort-polymorphic value declarations in module types *)
module type S = sig
  val foo : layout_ x y. ('a : x) ('b : y). 'a -> 'b
end
[%%expect{|
module type S = sig val foo : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b end
|}]

(* The following is error, because the module type goes through inclusion check
against itself, and unconstrained layout variables caused coercion, which is not
supported yet. *)
(* CR-soon zqian: once coercion is supported, the following should be allowed,
with omitted layout variables preserved. *)
module type S = sig
  val foo : layout_ x. ('a : value) ('b : value). 'a -> 'b
end
[%%expect{|
Line 1:
Error: Module type declarations do not match:
         module type S =
           sig val foo : layout_ l. ('a : value) ('b : value). 'a -> 'b end
       does not match
         module type S =
           sig val foo : layout_ l. ('a : value) ('b : value). 'a -> 'b end
       At position "module type S = <here>"
       Module types do not match:
         sig val foo : layout_ l. ('a : value) ('b : value). 'a -> 'b end
       is not equal to
         sig val foo : layout_ l. ('a : value) ('b : value). 'a -> 'b end
       At position "module type S = <here>"
       Values do not match:
         val foo : layout_ l. ('a : value) ('b : value). 'a -> 'b
       is not included in
         val foo : layout_ l. ('a : value) ('b : value). 'a -> 'b
       The layout parameter at position 1 in the first
       is instantiated with an unconstrained layout variable,
       which is not supported yet.
|}]

(* Name shadowing caused unused variables. Same issue as above. *)
(* CR-soon zqian: the test should pass with all variables preserved, once we support coercion. *)
module type S = sig
  val bar : layout_ x x. ('a : x) ('b : x). 'a -> 'b
  val baz : layout_ x x x. ('a : x) ('b : x). 'a -> 'b
end
[%%expect{|
Line 1:
Error: Module type declarations do not match:
         module type S =
           sig
             val bar : layout_ l l0. ('a : l0) ('b : l0). 'a -> 'b
             val baz : layout_ l l0 l1. ('a : l1) ('b : l1). 'a -> 'b
           end
       does not match
         module type S =
           sig
             val bar : layout_ l l0. ('a : l0) ('b : l0). 'a -> 'b
             val baz : layout_ l l0 l1. ('a : l1) ('b : l1). 'a -> 'b
           end
       At position "module type S = <here>"
       Module types do not match:
         sig
           val bar : layout_ l l0. ('a : l0) ('b : l0). 'a -> 'b
           val baz : layout_ l l0 l1. ('a : l1) ('b : l1). 'a -> 'b
         end
       is not equal to
         sig
           val bar : layout_ l l0. ('a : l0) ('b : l0). 'a -> 'b
           val baz : layout_ l l0 l1. ('a : l1) ('b : l1). 'a -> 'b
         end
       At position "module type S = <here>"
       Values do not match:
         val bar : layout_ l l0. ('a : l0) ('b : l0). 'a -> 'b
       is not included in
         val bar : layout_ l l0. ('a : l0) ('b : l0). 'a -> 'b
       The layout parameter at position 1 in the first
       is instantiated with an unconstrained layout variable,
       which is not supported yet.
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
         sig val f : layout_ l l0. ('a : l). 'a -> 'a end
       is not included in
         sig val f : layout_ l. ('a : l). 'a -> 'a end
       Values do not match:
         val f : layout_ l l0. ('a : l). 'a -> 'a
       is not included in
         val f : layout_ l. ('a : l). 'a -> 'a
       the first has 1 more layout parameter that is not used,
       which is not supported yet.
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
         sig val f : layout_ l. ('a : l). 'a -> 'a end
       is not included in
         sig val f : layout_ l l0. ('a : l). 'a -> 'a end
       Values do not match:
         val f : layout_ l. ('a : l). 'a -> 'a
       is not included in
         val f : layout_ l l0. ('a : l). 'a -> 'a
       the second has 1 more layout parameter that is not used,
       which is not supported yet.
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
         sig val f : layout_ l l0. ('a : l). 'a -> 'a end
       is not included in
         sig val f : layout_ l l0. ('b : l0). 'b -> 'b end
       Values do not match:
         val f : layout_ l l0. ('a : l). 'a -> 'a
       is not included in
         val f : layout_ l l0. ('b : l0). 'b -> 'b
       The layout parameter at position 1 in the first
       corresponds to the parameter at position 2 in the second,
       which is not supported yet.
|}]

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
         sig val f : layout_ l l0. ('a : l). 'a -> 'a end
       is not included in
         sig val f : layout_ l l0. ('b : l0). 'b -> 'b end
       Values do not match:
         val f : layout_ l l0. ('a : l). 'a -> 'a
       is not included in
         val f : layout_ l l0. ('b : l0). 'b -> 'b
       The layout parameter at position 1 in the first
       corresponds to the parameter at position 2 in the second,
       which is not supported yet.
|}]

(* some alpha renaming *)
module F1 (M : sig
  val f : layout_ x y. ('a : x) ('b : y). 'a -> 'b
end) : sig
  val f : layout_ p q. ('a : p) ('b : q). 'a -> 'b
end = M
[%%expect{|
module F1 :
  functor (M : sig val f : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b end) ->
    sig val f : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b end
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
         sig val f : layout_ l. ('a : l). 'a -> 'a end
       is not included in
         sig val f : 'a -> 'a end
       Values do not match:
         val f : layout_ l. ('a : l). 'a -> 'a
       is not included in
         val f : 'a -> 'a
       the first has 1 more layout parameter that is not used,
       which is not supported yet.
|}]

(* Ordering: both use first var on both sides - same position, should succeed *)
(* CR-soon zqian: same issue; should pass with coercion. *)
module FO1 (M : sig
  val f : layout_ x y. ('a : x) ('b : x). 'a -> 'b
end) : sig
  val f : layout_ p q. ('a : p) ('b : p). 'a -> 'b
end = M
[%%expect{|
Line 5, characters 6-7:
5 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : layout_ l l0. ('a : l) ('b : l). 'a -> 'b end
       is not included in
         sig val f : layout_ l l0. ('a : l) ('b : l). 'a -> 'b end
       Values do not match:
         val f : layout_ l l0. ('a : l) ('b : l). 'a -> 'b
       is not included in
         val f : layout_ l l0. ('a : l) ('b : l). 'a -> 'b
       The layout parameter at position 2 in the first
       is instantiated with an unconstrained layout variable,
       which is not supported yet.
|}]

module F (M : sig
  val f : layout_ x. ('a : x) ('b : x). 'a -> 'b
end) : sig
  val f : layout_ x. 'a -> 'b
end = M
[%%expect{|
Line 5, characters 6-7:
5 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : layout_ l. ('a : l) ('b : l). 'a -> 'b end
       is not included in
         sig val f : layout_ l. ('a : value) ('b : value). 'a -> 'b end
       Values do not match:
         val f : layout_ l. ('a : l) ('b : l). 'a -> 'b
       is not included in
         val f : layout_ l. ('a : value) ('b : value). 'a -> 'b
       The layout parameter at position 1 in the first
       is instantiated with layout "value",
       which is not supported yet.
|}]

(* Ordering: sort var in the same order, type var in different order, accepted. *)
module FO3(M : sig
  val f : layout_ p q. ('a : p) ('b : q). 'a -> 'b
end) : sig
  val f : layout_ p' q'. ('b : q') ('a : p'). 'a -> 'b
end = M
[%%expect{|
module FO3 :
  functor (M : sig val f : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b end) ->
    sig val f : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b end
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
         sig val f : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b end
       is not included in
         sig val f : layout_ l l0. ('a : l0) ('b : l). 'a -> 'b end
       Values do not match:
         val f : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b
       is not included in
         val f : layout_ l l0. ('a : l0) ('b : l). 'a -> 'b
       The layout parameter at position 1 in the first
       corresponds to the parameter at position 2 in the second,
       which is not supported yet.
|}]

(* layout_ in a general type annotation is not yet supported *)
let _ = (fun (x : layout_ a. ('t : a). 't) -> x)
[%%expect{|
Line 1, characters 18-41:
1 | let _ = (fun (x : layout_ a. ('t : a). 't) -> x)
                      ^^^^^^^^^^^^^^^^^^^^^^^
Error: Layout polymorphism is not supported in this context
|}]

let f : layout_ a. ('t : a). 't -> 't = fun x -> x
[%%expect{|
Line 1, characters 8-37:
1 | let f : layout_ a. ('t : a). 't -> 't = fun x -> x
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Layout polymorphism is not supported in this context
|}]

(* layout_ in a record field type is not yet supported *)
type t = { id : layout_ k. ('a : k). 'a -> 'a }
[%%expect{|
Line 1, characters 16-45:
1 | type t = { id : layout_ k. ('a : k). 'a -> 'a }
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Layout polymorphism is not supported in this context
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
  sig val bar : layout_ l. ('a : l mod contended) ('b : l). 'a -> 'b end
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
           val bar : layout_ l. ('a : l mod contended) ('b : l). 'a -> 'b
         end)
    ->
    sig
      val bar :
        layout_ l. ('a : l mod contended) ('b : l mod contended). 'a -> 'b
    end
|}]

(* CR zqian: the error message prints <genvar> because jkind error elaboration
happens outside of the jkind checking (where the genvar has a proper name like
[l]. *)
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
           val bar : layout_ l. ('a : l mod contended) ('b : l). 'a -> 'b
         end
       is not included in
         sig val bar : layout_ l. ('a : l) ('b : l). 'a -> 'b end
       Values do not match:
         val bar : layout_ l. ('a : l mod contended) ('b : l). 'a -> 'b
       is not included in
         val bar : layout_ l. ('a : l) ('b : l). 'a -> 'b
       The type "'a -> 'b" is not compatible with the type "'c -> 'd"
       The kind of 'a is <genvar>
         because of the definition of bar at line 4, characters 2-50.
       But the kind of 'a must be a subkind of <genvar> mod contended
         because of the definition of bar at line 2, characters 2-64.
|}]


module F (M : sig
  val bar : layout_ x. ('a : x) ('b : x). 'a -> 'b
end) : sig
  val bar : layout_ x. ('a : x mod contended) ('b : x). 'a -> 'b
end = M
[%%expect{|
module F :
  functor (M : sig val bar : layout_ l. ('a : l) ('b : l). 'a -> 'b end) ->
    sig val bar : layout_ l. ('a : l mod contended) ('b : l). 'a -> 'b end
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
             layout_ l.
               ('a : l mod contended) ('b : l mod contended). 'a -> 'b
         end
       is not included in
         sig
           val bar : layout_ l. ('a : l mod contended) ('b : l). 'a -> 'b
         end
       Values do not match:
         val bar :
           layout_ l. ('a : l mod contended) ('b : l mod contended). 'a -> 'b
       is not included in
         val bar : layout_ l. ('a : l mod contended) ('b : l). 'a -> 'b
       The type "'a -> 'b" is not compatible with the type "'a -> 'c"
       The kind of 'a is <genvar>
         because of the definition of bar at line 4, characters 2-64.
       But the kind of 'a must be a subkind of <genvar> mod contended
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
           val bar : layout_ l. ('a : l mod contended) ('b : l). 'a -> 'b
         end)
    ->
    sig
      val bar :
        layout_ l. ('a : l mod contended) ('b : l mod contended). 'a -> 'b
    end
|}]

(* "val poly_" syntax tests *)

(* Layout variables generated corresponding to free type variables *)
module type S = sig
  val poly_ foo1 : 'a -> 'b -> #('a * 'b)
end
[%%expect {|
module type S =
  sig val foo1 : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> #('a * 'b) end
|}]

(* Fresh layout variables assigned as kinds for type variables specifically
   introduced in the type scheme *)
module type S = sig
  val poly_ foo2 : 'a 'b. 'a -> 'b -> #('a * 'b)
end
[%%expect {|
module type S =
  sig val foo2 : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> #('a * 'b) end
|}]

(* Type scheme with a subset of polymorphic type variables that are explicitly
   forall-bound *)
module type S = sig
  val poly_ foo3 : 'a. 'a -> 'b -> #('a * 'b)
end
[%%expect {|
module type S =
  sig val foo3 : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> #('a * 'b) end
|}]

module type S = sig
  val poly_ foo4 : 'a. 'a -> 'b -> #('a * 'b)
end
[%%expect {|
module type S =
  sig val foo4 : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> #('a * 'b) end
|}]

(* Interaction between "val poly_" and "layout_". Currently errors.
   CR-soon aivaskovic: allow combining them after deciding what order layout
   variables should have inside "layout_". *)
module type S = sig
  val poly_ bar : layout_ x. ('b : x). 'a -> 'b -> #('a * 'b)
end
[%%expect {|
Line 2, characters 18-61:
2 |   val poly_ bar : layout_ x. ('b : x). 'a -> 'b -> #('a * 'b)
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The "layout_" keyword is not support inside layout-polymorphic
       value descriptions introduced using "val poly_".
|}]

(* Interaction with type variables that are constrained. *)
module type S = sig
  val poly_ baz1 : 'a ('b : immediate). 'a -> #('b * 'c) -> #('a * 'b * 'c)
end
[%%expect {|
module type S =
  sig
    val baz1 :
      layout_ l l0.
        ('a : l) ('b : immediate) ('c : l0).
          'a -> #('b * 'c) -> #('a * 'b * 'c)
  end
|}]

module type S = sig
  val poly_ baz2 : ('a : immediate) 'b. 'a -> #('b * 'c) -> #('a * 'b * 'c)
end
[%%expect {|
module type S =
  sig
    val baz2 :
      layout_ l l0.
        ('a : immediate) ('b : l) ('c : l0).
          'a -> #('b * 'c) -> #('a * 'b * 'c)
  end
|}]

module type S = sig
  val poly_ baz3 : ('a : immediate) 'b. 'b -> #('a * 'c) -> #('b * 'a * 'c)
end
[%%expect {|
module type S =
  sig
    val baz3 :
      layout_ l l0.
        ('b : l) ('a : immediate) ('c : l0).
          'b -> #('a * 'c) -> #('b * 'a * 'c)
  end
|}]

module type S = sig
  val poly_ baz4 : ('a : immediate) 'b 'c. 'b -> #('a * 'c) -> #('b * 'a * 'c)
end
[%%expect {|
module type S =
  sig
    val baz4 :
      layout_ l l0.
        ('b : l) ('a : immediate) ('c : l0).
          'b -> #('a * 'c) -> #('b * 'a * 'c)
  end
|}]

module type S = sig
  val poly_ baz5 : ('a : immediate). 'b -> #('a * 'c) -> #('b * 'a * 'c)
end
[%%expect {|
module type S =
  sig
    val baz5 :
      layout_ l l0.
        ('b : l) ('a : immediate) ('c : l0).
          'b -> #('a * 'c) -> #('b * 'a * 'c)
  end
|}]

(* "value" is special and usually a default.
   It is not a default inside "val poly_". *)
module type S = sig
  val poly_ baz5 : ('a : value) 'b. 'a -> #('a * 'b) -> #('a * 'b)
end
[%%expect {|
module type S =
  sig
    val baz5 :
      layout_ l. ('a : value) ('b : l). 'a -> #('a * 'b) -> #('a * 'b)
  end
|}]

(* "value_or_null" should be printed *)
module type S = sig
  val poly_ baz6 : ('a : value_or_null) 'b. 'a -> #('a * 'b) -> #('a * 'b)
end
[%%expect {|
module type S =
  sig
    val baz6 :
      layout_ l.
        ('a : value_or_null) ('b : l). 'a -> #('a * 'b) -> #('a * 'b)
  end
|}]

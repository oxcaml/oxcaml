(* TEST
 flags = "-extension layout_poly_alpha";
 expect;
*)

(** layout-polymorphic identity **)

module type Id = sig
  val poly_ id : 'a -> 'a
end;;
[%%expect{|
module type Id = sig val poly_ id : 'a -> 'a end
|}];;

(module struct
  let poly_ id x = x
end : Id)
[%%expect{|
- : (module Id) = <module>
|}];;

(module struct
  let id x = x
end : Id)
[%%expect{|
Lines 1-3, characters 8-3:
1 | ........struct
2 |   let id x = x
3 | end......
Error: Signature mismatch:
       Modules do not match: sig val id : 'a -> 'a end is not included in Id
       Values do not match:
         val id : 'a -> 'a
       is not included in
         val poly_ id : 'a -> 'a
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The type "'b" is layout polymorphic,
       but "'a" is not layout polymorphic.
|}];;

(module struct
  let id (x : (_ : value)) = x
end : Id)
[%%expect{|
Lines 1-3, characters 8-3:
1 | ........struct
2 |   let id (x : (_ : value)) = x
3 | end......
Error: Signature mismatch:
       Modules do not match: sig val id : 'a -> 'a end is not included in Id
       Values do not match:
         val id : 'a -> 'a
       is not included in
         val poly_ id : 'a -> 'a
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The kind of 'a is value
         because of the definition of id at line 4, characters 2-25.
       But the kind of 'a must be a subkind of value
         because of the definition of id at line 2, characters 9-30.
|}];;

(** layout-polymorphic K combinator **)

module type K = sig
  val poly_ k : 'a -> 'b -> 'a
end
[%%expect{|
module type K = sig val poly_ k : 'a -> 'b -> 'a end
|}];;

(module struct
  let poly_ k x y = x
end : K)
[%%expect{|
- : (module K) = <module>
|}];;

(module struct
  let ignore _ = ()
  let poly_ k x y = ignore x; x
end : K)
[%%expect{|
Lines 1-4, characters 8-3:
1 | ........struct
2 |   let ignore _ = ()
3 |   let poly_ k x y = ignore x; x
4 | end.....
Error: Signature mismatch:
       Modules do not match:
         sig val ignore : 'a -> unit val poly_ k : 'a. 'a -> 'b -> 'a end
       is not included in
         K
       Values do not match:
         val poly_ k : 'a. 'a -> 'b -> 'a
       is not included in
         val poly_ k : 'a -> 'b -> 'a
       The type "'a -> 'b -> 'a" is not compatible with the type "'c -> 'd -> 'c"
       The type "'c" is layout polymorphic,
       but "'a" is not layout polymorphic.
|}];;

(module struct
  let ignore _ = ()
  let poly_ k x y = ignore y; x
end : K)
[%%expect{|
Lines 1-4, characters 8-3:
1 | ........struct
2 |   let ignore _ = ()
3 |   let poly_ k x y = ignore y; x
4 | end.....
Error: Signature mismatch:
       Modules do not match:
         sig val ignore : 'a -> unit val poly_ k : 'b. 'a -> 'b -> 'a end
       is not included in
         K
       Values do not match:
         val poly_ k : 'b. 'a -> 'b -> 'a
       is not included in
         val poly_ k : 'a -> 'b -> 'a
       The type "'a -> 'b -> 'a" is not compatible with the type "'a -> 'c -> 'a"
       The type "'c" is layout polymorphic,
       but "'b" is not layout polymorphic.
|}];;

(module struct
  let k x y = x
end : K)
[%%expect{|
Lines 1-3, characters 8-3:
1 | ........struct
2 |   let k x y = x
3 | end.....
Error: Signature mismatch:
       Modules do not match:
         sig val k : 'a -> 'b -> 'a end
       is not included in
         K
       Values do not match:
         val k : 'a -> 'b -> 'a
       is not included in
         val poly_ k : 'a -> 'b -> 'a
       The type "'a -> 'b -> 'a" is not compatible with the type "'c -> 'd -> 'c"
       The type "'c" is layout polymorphic,
       but "'a" is not layout polymorphic.
|}];;

(** partially layout-polymorphic K combinator **)

module type K = sig
  val k : layout_ x. ('a : x). 'a -> 'b -> 'a
end
[%%expect{|
module type K = sig val poly_ k : 'b. 'a -> 'b -> 'a end
|}];;

(* CR-soon jbachurski: This is an error due to incomplete translation,
   indicating the inclusion check succeeded as expected. *)
(module struct
  let poly_ k x y = x
end : K)
[%%expect{|
Lines 1-3, characters 8-3:
1 | ........struct
2 |   let poly_ k x y = x
3 | end.....
Error: Coercing this module constructs a new layout-polymorphic value,
       which is not supported yet.
|}];;

(module struct
  let ignore _ = ()
  let poly_ k x y = ignore x; x
end : K)
[%%expect{|
Lines 1-4, characters 8-3:
1 | ........struct
2 |   let ignore _ = ()
3 |   let poly_ k x y = ignore x; x
4 | end.....
Error: Signature mismatch:
       Modules do not match:
         sig val ignore : 'a -> unit val poly_ k : 'a. 'a -> 'b -> 'a end
       is not included in
         K
       Values do not match:
         val poly_ k : 'a. 'a -> 'b -> 'a
       is not included in
         val poly_ k : 'b. 'a -> 'b -> 'a
       The type "'a -> 'b -> 'a" is not compatible with the type "'c -> 'd -> 'c"
       The type "'c" is layout polymorphic,
       but "'a" is not layout polymorphic.
|}];;

(module struct
  let ignore _ = ()
  let poly_ k x y = ignore y; x
end : K)
[%%expect{|
- : (module K) = <module>
|}];;

(module struct
  let k x y = x
end : K)
[%%expect{|
Lines 1-3, characters 8-3:
1 | ........struct
2 |   let k x y = x
3 | end.....
Error: Signature mismatch:
       Modules do not match:
         sig val k : 'a -> 'b -> 'a end
       is not included in
         K
       Values do not match:
         val k : 'a -> 'b -> 'a
       is not included in
         val poly_ k : 'b. 'a -> 'b -> 'a
       The type "'a -> 'b -> 'a" is not compatible with the type "'c -> 'd -> 'c"
       The type "'c" is layout polymorphic,
       but "'a" is not layout polymorphic.
|}];;

(** Product layout with polymorphic components **)

(* CR-soon jbachurski: This is a hack necessary to express a product layout
   containing a generic sort variable, because we currently cannot write
   [l & _] for [l] introduced by [layout_ l]. *)
module type Id = module type of struct
  let poly_ id (x : (_ : any & any)) = x
end;;
[%%expect{|
module type Id =
  sig val id : layout_ l l0. ('a : l & l0). 'a -> 'a @@ stateless end
|}];;

(* CR-soon jbachurski: This is an error due to incomplete translation,
   indicating the inclusion check succeeded as expected. *)
(module struct
  let poly_ id x = x
end : Id)
[%%expect{|
Lines 1-3, characters 8-3:
1 | ........struct
2 |   let poly_ id x = x
3 | end......
Error: Coercing this module constructs a new layout-polymorphic value,
       which is not supported yet.
|}];;

(module struct
  let id x = x
end : Id)
[%%expect{|
Lines 1-3, characters 8-3:
1 | ........struct
2 |   let id x = x
3 | end......
Error: Signature mismatch:
       Modules do not match:
         sig
           val id :
             ('a : '_representable_layout_4 & '_representable_layout_5).
               'a -> 'a
         end
       is not included in
         Id
       Values do not match:
         val id :
           ('a : '_representable_layout_4 & '_representable_layout_5).
             'a -> 'a
       is not included in
         val id : layout_ l l0. ('a : l & l0). 'a -> 'a @@ stateless
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The type "'b" is layout polymorphic,
       but "'a" is not layout polymorphic.
|}];;

(* Alternative construction with a product _sort_ *)

module Eq = struct
  let poly_ k x y =
    let ignore _ = () in
    ignore x; ignore y;
    x
end
[%%expect{|
module Eq : sig val k : layout_ l. ('a : l) ('b : l). 'a -> 'b -> 'a end
|}];;

module type Id = module type of struct
  let poly_ id x =
    let _ = (fun a b -> Eq.k #(a, b) x) in
    x
end
[%%expect{|
module type Id =
  sig val id : layout_ l l0. ('a : l & l0). 'a -> 'a @@ stateless end
|}];;

(* CR-soon jbachurski: This is an error due to incomplete translation,
   indicating the inclusion check succeeded as expected. *)
(module struct
  let poly_ id x = x
end : Id)
[%%expect{|
Lines 1-3, characters 8-3:
1 | ........struct
2 |   let poly_ id x = x
3 | end......
Error: Coercing this module constructs a new layout-polymorphic value,
       which is not supported yet.
|}];;

(module struct
  let id x = x
end : Id)
[%%expect{|
Lines 1-3, characters 8-3:
1 | ........struct
2 |   let id x = x
3 | end......
Error: Signature mismatch:
       Modules do not match: sig val id : 'a -> 'a end is not included in Id
       Values do not match:
         val id : 'a -> 'a
       is not included in
         val id : layout_ l l0. ('a : l & l0). 'a -> 'a @@ stateless
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The type "'b" is layout polymorphic,
       but "'a" is not layout polymorphic.
|}];;

(** Product layout with one polymorphic component **)

module type Id = module type of struct
  let poly_ id (x : (_ : value & any)) = x
end;;
[%%expect{|
module type Id =
  sig val id : layout_ l. ('a : value & l). 'a -> 'a @@ stateless end
|}];;

(* CR-soon jbachurski: This is an error due to incomplete translation,
   indicating the inclusion check succeeded as expected. *)
(module struct
  let poly_ id x = x
end : Id)
[%%expect{|
Lines 1-3, characters 8-3:
1 | ........struct
2 |   let poly_ id x = x
3 | end......
Error: Coercing this module constructs a new layout-polymorphic value,
       which is not supported yet.
|}];;

(module struct
  let poly_ id (x : (_ : any & value)) = x
end : Id)
[%%expect{|
Lines 1-3, characters 8-3:
1 | ........struct
2 |   let poly_ id (x : (_ : any & value)) = x
3 | end......
Error: Signature mismatch:
       Modules do not match:
         sig val id : layout_ l. ('a : l & value). 'a -> 'a end
       is not included in
         Id
       Values do not match:
         val id : layout_ l. ('a : l & value). 'a -> 'a
       is not included in
         val id : layout_ l. ('a : value & l). 'a -> 'a @@ stateless
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The layout of 'a is value & value_or_null
         because of the definition at line 4, characters 15-42.
       But the layout of 'a must be a sublayout of value_or_null & value
         because of the definition at line 2, characters 15-42.
|}];;

(module struct
  let poly_ id (x : (_ : value & any)) = x
end : Id)
[%%expect{|
- : (module Id) = <module>
|}];;

(** Product _type_ with one polymorphic component **)

module type Id = sig
  val poly_ id : #('a * int) -> #('a * int)
end
[%%expect{|
module type Id = sig val poly_ id : #('a * int) -> #('a * int) end
|}];;

(* CR-soon jbachurski: This is an error due to incomplete translation,
   indicating the inclusion check succeeded as expected. *)
(module struct
  let poly_ id x = x
end : Id)
[%%expect{|
Lines 1-3, characters 8-3:
1 | ........struct
2 |   let poly_ id x = x
3 | end......
Error: Coercing this module constructs a new layout-polymorphic value,
       which is not supported yet.
|}];;

(module struct
  let id x = x
end : Id)
[%%expect{|
Lines 1-3, characters 8-3:
1 | ........struct
2 |   let id x = x
3 | end......
Error: Signature mismatch:
       Modules do not match:
         sig
           val id :
             ('a : '_representable_layout_7 & '_representable_layout_8).
               'a -> 'a
         end
       is not included in
         Id
       Values do not match:
         val id :
           ('a : '_representable_layout_7 & '_representable_layout_8).
             'a -> 'a
       is not included in
         val poly_ id : #('a * int) -> #('a * int)
       The type "'a -> 'a" is not compatible with the type
         "#('b * int) -> #('b * int)"
       The type "#('b * int)" is layout polymorphic,
       but "'a" is not layout polymorphic.
|}];;

(** Product layout with weakly- and generically-polymorphic components **)

(* Succeeds: the instantiation sets type of [a] to [int],
   and its layout was weakly polymorphic which was sufficient. *)
module Test (A : sig
  val choose : layout_ l. ('a : l) ('b : l). 'a -> 'b -> 'b
end @ static) = struct
  let id a = a

  module P = struct
    let poly_ f x a b = Eq.k x #(id a, b)
  end

  module S = struct
    let poly_ f x (a : int) b = Eq.k x #(a, b)
  end

  module Check : module type of S = P
end
[%%expect{|
module Test :
  functor
    (A : sig val choose : layout_ l. ('a : l) ('b : l). 'a -> 'b -> 'b end @ static)
    ->
    sig
      val id : 'a -> 'a
      module P :
        sig
          val f :
            layout_ l.
              ('a : value_or_null & l) 'b ('c : l). 'a -> 'b -> 'c -> 'a
        end
      module S :
        sig
          val f :
            layout_ l.
              ('a : value_or_null & l) ('b : l). 'a -> int -> 'b -> 'a
            @@ stateless
        end
      module Check :
        sig
          val f :
            layout_ l.
              ('a : value_or_null & l) ('b : l). 'a -> int -> 'b -> 'a
            @@ stateless
        end
    end
|}];;

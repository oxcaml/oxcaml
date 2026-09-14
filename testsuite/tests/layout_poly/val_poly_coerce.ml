(* TEST
 flags = "-extension layout_poly_alpha";
 expect.opt;
*)

external to_float : float# -> float = "%box_float"
external to_int32 : int32_u -> int32 = "%box_int32"
external to_int64 : int64_u -> int64 = "%box_int64"
[%%expect {|
external to_float : float# -> float = "%box_float"
external to_int32 : int32_u -> int32 = "%box_int32"
external to_int64 : int64_u -> int64 = "%box_int64"
|}];;

(** coercions on the K combinator [fst] **)

module Fst = struct
  let poly_ fst x y = x
end;;
[%%expect {|
module Fst : sig val poly_ fst : 'a -> 'b -> 'a end
|}];;

(* no coercion *)
module M : sig
  val poly_ fst : 'a -> 'b -> 'a
end = Fst;;
(M.fst 42 #1337l, M.fst #3.14 #1337l |> to_float)
[%%expect {|
module M : sig val poly_ fst : 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}];;

(* instantiating coercions *)

module M1 : sig
  val poly_ fst : ('a : float64). 'a -> 'b -> 'a
end = Fst;;
(M1.fst #3.14 1337 |> to_float, M1.fst #3.14 #1337l |> to_float)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module M2 : sig
  val poly_ fst : ('b : bits32). 'a -> 'b -> 'a
end = Fst;;
(M2.fst 42 #1337l, M2.fst #3.14 #1337l |> to_float)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module M12 : sig
  val fst : ('a : float64) ('b : bits32). 'a -> 'b -> 'a
end = Fst;;
(M12.fst #3.14 #1337l |> to_float)
[%%expect {|
module M12 : sig val fst : ('a : float64) ('b : bits32). 'a -> 'b -> 'a end
- : float = 3.14
|}];;

(* eta-expanding coercions *)

module M0 : sig
  val fst : layout_ z x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end = Fst;;
(M0.fst 42 #1337l, M0.fst #3.14 #1337l |> to_float)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module M1 : sig
  val fst : layout_ x z y. ('a : x) ('b : y). 'a -> 'b -> 'a
end = Fst;;
(M1.fst 42 #1337l, M1.fst #3.14 #1337l |> to_float)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module M2 : sig
  val fst : layout_ x y z. ('a : x) ('b : y). 'a -> 'b -> 'a
end = Fst;;
(M2.fst 42 #1337l, M2.fst #3.14 #1337l |> to_float)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

(* eta-expanding and instantiating coercion *)

module M12 : sig
  val fst : layout_ z. ('a : float64) ('b : bits32). 'a -> 'b -> 'a
end = Fst;;
(M12.fst #3.14 #1337l |> to_float)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

(* chaining instantiating coercions *)

module M12 = ((Fst : sig
  val poly_ fst : ('a : float64). 'a -> 'b -> 'a
end) : sig
  val fst : ('a : float64) ('b : bits32). 'a -> 'b -> 'a
end);;
(M12.fst #3.14 #1337l |> to_float)
[%%expect {|
module M12 : sig val fst : ('a : float64) ('b : bits32). 'a -> 'b -> 'a end
- : float = 3.14
|}];;

module M21 = ((Fst : sig
  val poly_ fst : ('b : bits32). 'a -> 'b -> 'a
end) : sig
  val fst : ('a : float64) ('b : bits32). 'a -> 'b -> 'a
end);;
(M21.fst #3.14 #1337l |> to_float)
[%%expect {|
module M21 : sig val fst : ('a : float64) ('b : bits32). 'a -> 'b -> 'a end
- : float = 3.14
|}];;

(* chaining eta-expanding coercions *)

module M23 = ((Fst : sig
  val fst : layout_ x y z. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : layout_ x y z w. ('a : x) ('b : y). 'a -> 'b -> 'a
end);;
(M23.fst 42 #1337l, M23.fst #3.14 #1337l |> to_float)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module M20 = ((Fst : sig
  val fst : layout_ x y z. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : layout_ w x y z. ('a : x) ('b : y). 'a -> 'b -> 'a
end);;
(M20.fst 42 #1337l, M20.fst #3.14 #1337l |> to_float)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module M02 = ((Fst : sig
  val fst : layout_ w x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : layout_ w x y z. ('a : x) ('b : y). 'a -> 'b -> 'a
end);;
(M02.fst 42 #1337l, M02.fst #3.14 #1337l |> to_float)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module M00 = ((Fst : sig
  val fst : layout_ z x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : layout_ z w x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end);;
(M00.fst 42 #1337l, M00.fst #3.14 #1337l |> to_float)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

(* chaining eta-expanding and eta-reducing coercions *)

module M2 = ((Fst : sig
  val fst : layout_ x y z. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : layout_ x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end);;
(M2.fst 42 #1337l, M2.fst #3.14 #1337l |> to_float)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module M0 = ((Fst : sig
  val fst : layout_ z x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : layout_ x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end);;
(M0.fst 42 #1337l, M0.fst #3.14 #1337l |> to_float)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

(* chaining eta-expanding and instantiating coercions *)

module M2 = ((Fst : sig
  val fst : layout_ x y z. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : ('a : float64) ('b : bits32). 'a -> 'b -> 'a
end);;
(M2.fst #3.14 #1337l |> to_float)
[%%expect {|
module M2 : sig val fst : ('a : float64) ('b : bits32). 'a -> 'b -> 'a end
- : float = 3.14
|}];;

module M0 = ((Fst : sig
  val fst : layout_ z x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : ('a : float64) ('b : bits32). 'a -> 'b -> 'a
end);;
(M0.fst #3.14 #1337l |> to_float)
[%%expect {|
module M0 : sig val fst : ('a : float64) ('b : bits32). 'a -> 'b -> 'a end
- : float = 3.14
|}];;

(* chaining eta-expanding and eta-reducing coercions (= identity) *)

module M0 = ((Fst : sig
  val fst : layout_ z x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : layout_ x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end);;
(M0.fst 42 #1337l, M0.fst #3.14 #1337l |> to_float)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module M1 = ((Fst : sig
  val fst : layout_ x z y. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : layout_ x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end);;
(M1.fst 42 #1337l, M1.fst #3.14 #1337l |> to_float)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module M2 = ((Fst : sig
  val fst : layout_ x y z. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : layout_ x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end);;
(M2.fst 42 #1337l, M2.fst #3.14 #1337l |> to_float)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

(** coercions on the 3-tupling function [triple] **)

let box_triple #(x, y, z) = (x, y, z)
let box_triple' #(x, y, z) = (to_float x, to_int32 y, to_int64 z)
module Triple = struct
  let poly_ triple x y z = #(x, y, z)
end;;
(Triple.triple 3.14 1337 42 |> box_triple, Triple.triple #3.14 #1337l #42L |> box_triple')
[%%expect {|
val box_triple : #('a * 'b * 'c) -> 'a * 'b * 'c = <fun>
val box_triple' : #(float# * int32_u * int64_u) -> float * int32 * int64 =
  <fun>
module Triple : sig val poly_ triple : 'a -> 'b -> 'c -> #('a * 'b * 'c) end
- : (float * int * int) * (float * int32 * int64) =
((3.14, 1337, 42), (3.14, 1337l, 42L))
|}];;

(* transposing coercions *)

module M123 : sig
  val triple :
    layout_ x y z. ('a : x) ('b : y) ('c : z). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end = Triple;;
(M123.triple 3.14 1337 42 |> box_triple, M123.triple #3.14 #1337l #0L |> box_triple')
[%%expect {|
module M123 : sig val poly_ triple : 'a -> 'b -> 'c -> #('a * 'b * 'c) end
- : (float * int * int) * (float * int32 * int64) =
((3.14, 1337, 42), (3.14, 1337l, 0L))
|}];;

module M132 : sig
  val triple :
    layout_ x y z. ('a : x) ('b : z) ('c : y). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end = Triple;;
(M132.triple 3.14 1337 42 |> box_triple, M132.triple #3.14 #1337l #0L |> box_triple')
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module M213 : sig
  val triple :
    layout_ x y z. ('a : y) ('b : x) ('c : z). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end = Triple;;
(M213.triple 3.14 1337 42 |> box_triple, M213.triple #3.14 #1337l #0L |> box_triple')
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module M231 : sig
  val triple :
    layout_ x y z. ('a : y) ('b : z) ('c : x). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end = Triple;;
(M231.triple 3.14 1337 42 |> box_triple, M231.triple #3.14 #1337l #0L |> box_triple')
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module M312 : sig
  val triple :
    layout_ x y z. ('a : z) ('b : x) ('c : y). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end = Triple;;
(M312.triple 3.14 1337 42 |> box_triple, M312.triple #3.14 #1337l #0L |> box_triple')
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module M321 : sig
  val triple :
    layout_ x y z. ('a : z) ('b : y) ('c : x). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end = Triple;;
(M321.triple 3.14 1337 42 |> box_triple, M321.triple #3.14 #1337l #0L |> box_triple')
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

(* chaining transposing coercions *)

module M_132_132 = ((Triple : sig
  val triple :
    layout_ x y z. ('a : x) ('b : z) ('c : y). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end) : sig
  val triple :
    layout_ x y z. ('a : x) ('b : z) ('c : y). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end);;
(M_132_132.triple 3.14 1337 42 |> box_triple, M_132_132.triple #3.14 #1337l #0L |> box_triple')
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module M_321_321 = ((Triple : sig
  val triple :
    layout_ x y z. ('a : z) ('b : y) ('c : x). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end) : sig
  val triple :
    layout_ x y z. ('a : z) ('b : y) ('c : x). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end);;
(M_321_321.triple 3.14 1337 42 |> box_triple, M_321_321.triple #3.14 #1337l #0L |> box_triple')
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module M_312_312 = ((Triple : sig
  val triple :
    layout_ x y z. ('a : z) ('b : x) ('c : y). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end) : sig
  val triple :
    layout_ x y z. ('a : z) ('b : x) ('c : y). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end);;
(M_312_312.triple 3.14 1337 42 |> box_triple, M_312_312.triple #3.14 #1337l #0L |> box_triple')
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module M_312_321 = ((Triple : sig
  val triple :
    layout_ x y z. ('a : z) ('b : x) ('c : y). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end) : sig
  val triple :
    layout_ x y z. ('a : z) ('b : y) ('c : x). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end);;
(M_312_321.triple 3.14 1337 42 |> box_triple, M_312_321.triple #3.14 #1337l #0L |> box_triple')
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

(** coercions on the [%opaque] primitive **)

module Id = struct
  external [@layout_poly] id : ('a : any). 'a -> 'a = "%opaque"
end;;
[%%expect {|
module Id :
  sig external id : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly] end
|}];;

(* instantiating coercion (no layout polymorphism) *)
module M : sig
  val id : ('a : float64). 'a -> 'a
end = Id;;
(M.id #3.14 |> to_float)
[%%expect {|
module M : sig val id : ('a : float64). 'a -> 'a end
- : float = 3.14
|}];;

(* layout-poly coercion *)
module M : sig
  val poly_ id : 'a -> 'a
end = Id;;
(M.id 42, M.id #3.14 |> to_float)
[%%expect {|
Line 3, characters 6-8:
3 | end = Id;;
          ^^
Error: Signature mismatch:
       Modules do not match:
         sig
           external id : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly]
         end
       is not included in
         sig val poly_ id : 'a -> 'a end
       Values do not match:
         external id : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly]
       is not included in
         val poly_ id : 'a -> 'a
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The kind of 'a is 's8 separable non_null
         because of the definition of id at line 2, characters 2-25.
       But the kind of 'a must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}];;

(* eta-expanding coercion *)
module M1 : sig
  val id : layout_ x y. ('a : x). 'a -> 'a
end = Id;;
(M1.id 42, M1.id #3.14 |> to_float)
[%%expect {|
Line 3, characters 6-8:
3 | end = Id;;
          ^^
Error: Signature mismatch:
       Modules do not match:
         sig
           external id : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly]
         end
       is not included in
         sig val id : layout_ l l0. ('a : l). 'a -> 'a end
       Values do not match:
         external id : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly]
       is not included in
         val id : layout_ l l0. ('a : l). 'a -> 'a
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The kind of 'a is 's9
         because of the definition of id at line 2, characters 2-42.
       But the kind of 'a must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}];;
module M0 : sig
  val id : layout_ y x. ('a : x). 'a -> 'a
end = Id;;
(M0.id 42, M0.id #3.14 |> to_float)
[%%expect {|
Line 3, characters 6-8:
3 | end = Id;;
          ^^
Error: Signature mismatch:
       Modules do not match:
         sig
           external id : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly]
         end
       is not included in
         sig val id : layout_ l l0. ('a : l0). 'a -> 'a end
       Values do not match:
         external id : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly]
       is not included in
         val id : layout_ l l0. ('a : l0). 'a -> 'a
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The kind of 'a is 's10
         because of the definition of id at line 2, characters 2-42.
       But the kind of 'a must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}];;

(* instantiating and eta-expanding coercion *)
module M : sig
  val id : layout_ x. ('a : bits64). 'a -> 'a
end = Id;;
(M.id #42L |> to_int64)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;
module M : sig
  val id : layout_ x y. ('a : bits64). 'a -> 'a
end = Id;;
(M.id #42L |> to_int64)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

(* chaining primitive and instantiating coercions *)
module M = ((Id : sig
  val id : layout_ x. ('a : x). 'a -> 'a
end) : sig
  val id : ('a : bits64). 'a -> 'a
end);;
(M.id #42L |> to_int64)
[%%expect {|
Line 1, characters 13-15:
1 | module M = ((Id : sig
                 ^^
Error: Signature mismatch:
       Modules do not match:
         sig
           external id : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly]
         end
       is not included in
         sig val poly_ id : 'a -> 'a end
       Values do not match:
         external id : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly]
       is not included in
         val poly_ id : 'a -> 'a
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The kind of 'a is 's11
         because of the definition of id at line 2, characters 2-40.
       But the kind of 'a must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}];;

(* chaining non-polymorphic primitive and eta-expanding coercions *)
module M = ((Id : sig
  val id : ('a : bits64). 'a -> 'a
end) : sig
  val id : layout_ x. ('a : bits64). 'a -> 'a
end);;
(M.id #42L |> to_int64)
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

(* chaining primitive, eta-expanding and eta-reducing coercions *)
module M0 = ((Id : sig
  val id : layout_ y x. ('a : x). 'a -> 'a
end) : sig
  val id : layout_ x. ('a : x). 'a -> 'a
end);;
(M0.id 42, M0.id #3.14 |> to_float)
[%%expect {|
Line 1, characters 14-16:
1 | module M0 = ((Id : sig
                  ^^
Error: Signature mismatch:
       Modules do not match:
         sig
           external id : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly]
         end
       is not included in
         sig val id : layout_ l l0. ('a : l0). 'a -> 'a end
       Values do not match:
         external id : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly]
       is not included in
         val id : layout_ l l0. ('a : l0). 'a -> 'a
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The kind of 'a is 's12
         because of the definition of id at line 2, characters 2-42.
       But the kind of 'a must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}];;

module M1 = ((Id : sig
  val id : layout_ x y. ('a : x). 'a -> 'a
end) : sig
  val id : layout_ x. ('a : x). 'a -> 'a
end);;
(M1.id 42, M1.id #3.14 |> to_float)
[%%expect {|
Line 1, characters 14-16:
1 | module M1 = ((Id : sig
                  ^^
Error: Signature mismatch:
       Modules do not match:
         sig
           external id : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly]
         end
       is not included in
         sig val id : layout_ l l0. ('a : l). 'a -> 'a end
       Values do not match:
         external id : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly]
       is not included in
         val id : layout_ l l0. ('a : l). 'a -> 'a
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The kind of 'a is 's13
         because of the definition of id at line 2, characters 2-42.
       But the kind of 'a must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}];;


(** Subtyping coercions on first-class modules with layout-polymorphic items **)

module type M_ab = sig val poly_ f : 'a -> 'b end
module type M_aa = sig val poly_ f : 'a -> 'a end
module type M_aF = sig val poly_ f : ('b : float64). 'a -> 'b end
module type M_FF = sig val f : ('a : float64). 'a -> 'a end
module type M_aa' = sig val f : layout_ x y. ('a : x separable non_null). 'a -> 'a end
[%%expect {|
module type M_ab = sig val poly_ f : 'a -> 'b end
module type M_aa = sig val poly_ f : 'a -> 'a end
module type M_aF = sig val poly_ f : ('b : float64). 'a -> 'b end
module type M_FF = sig val f : ('a : float64). 'a -> 'a end
module type M_aa' = sig val f : layout_ l l0. ('a : l). 'a -> 'a end
|}];;

let f x = (x : (module M_ab) :> (module M_ab))
[%%expect {|
val f : (module M_ab) -> (module M_ab) = <fun>
|}];;

let f x = (x : (module M_ab) :> (module M_aa))
[%%expect {|
Line 1, characters 10-46:
1 | let f x = (x : (module M_ab) :> (module M_aa))
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(module M_ab)" is not a subtype of "(module M_aa)"
       The two first-class module types differ by a coercion of
       kind templates.
|}];;

let f x = (x : (module M_ab) :> (module M_aF))
[%%expect {|
Line 1, characters 10-46:
1 | let f x = (x : (module M_ab) :> (module M_aF))
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(module M_ab)" is not a subtype of "(module M_aF)"
       The two first-class module types differ by a coercion of
       kind templates.
|}];;

let f x = (x : (module M_aF) :> (module M_FF))
[%%expect {|
Line 1, characters 10-46:
1 | let f x = (x : (module M_aF) :> (module M_FF))
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(module M_aF)" is not a subtype of "(module M_FF)"
       The two first-class module types differ by a coercion of
       kind templates.
|}];;

let f x = (x : (module M_aa) :> (module M_FF))
[%%expect {|
Line 1, characters 10-46:
1 | let f x = (x : (module M_aa) :> (module M_FF))
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(module M_aa)" is not a subtype of "(module M_FF)"
       The two first-class module types differ by a coercion of
       kind templates.
|}];;


(** Kind-template instantiations in a coercion require a [static] module **)

module F_inst_dynamic (M : sig
  val poly_ id : 'a -> 'a
end @ dynamic) : sig
  val id : 'a -> 'a
end = M
[%%expect {|
Line 7, characters 6-7:
7 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val poly_ id : 'a -> 'a end @ dynamic
       is not included in
         sig val id : 'a -> 'a end @ dynamic
       Values do not match:
         val poly_ id : 'a -> 'a (* in a structure at dynamic *)
       is not included in
         val id : 'a -> 'a (* in a structure at dynamic *)
       The first is "dynamic"
       but the second is "static"
         because it is layout-polymorphic and being instantiated here.
|}];;

module F_inst_static (M : sig
  val poly_ id : 'a -> 'a
end @ static) : sig
  val id : 'a -> 'a
end = M
[%%expect {|
module F_inst_static :
  functor (M : sig val poly_ id : 'a -> 'a end @ static) ->
    sig val id : 'a -> 'a end
|}];;

module F_eta_dynamic (M : sig
  val id : 'a -> 'a
end @ dynamic) : sig
  val id : layout_ x. 'a -> 'a
end = M
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module F_eta_static (M : sig
  val id : 'a -> 'a
end @ static) : sig
  val id : layout_ x. 'a -> 'a
end = M
[%%expect {|
>> Fatal error: Translmod.apply_kindtemplate_coercion:
coercions returning kind templates are not yet supported
Uncaught exception: Misc.Fatal_error

|}];;

module F_inst_static_but_item_dynamic (M : sig
  val poly_ id : 'a -> 'a @@ dynamic
  val x : int @@ dynamic
end @ static) : sig
  val id : 'a -> 'a
end = M
[%%expect {|
Line 6, characters 6-7:
6 | end = M
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val poly_ id : 'a -> 'a @@ dynamic val x : int @@ dynamic end @ static
       is not included in
         sig val id : 'a -> 'a end @ dynamic
       Values do not match:
         val poly_ id : 'a -> 'a @@ dynamic (* in a structure at static *)
       is not included in
         val id : 'a -> 'a (* in a structure at dynamic *)
       The first is "dynamic"
       but the second is "static"
         because it is layout-polymorphic and being instantiated here.
|}];;

module F_inst_static_but_other_dynamic (M : sig
  val poly_ id : 'a -> 'a
  val x : int @@ dynamic
end @ static) : sig
  val id : 'a -> 'a
end = M
[%%expect {|
module F_inst_static_but_other_dynamic :
  functor
    (M : sig val poly_ id : 'a -> 'a val x : int @@ dynamic end @ static) ->
    sig val id : 'a -> 'a end
|}];;

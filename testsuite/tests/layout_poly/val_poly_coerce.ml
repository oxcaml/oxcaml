(* TEST
 flags = "-extension layout_poly_alpha";
 expect.opt;
*)

external to_float : float# -> float = "%box_float"
external to_int32 : int32# -> int32 = "%box_int32"
external to_int64 : int64# -> int64 = "%box_int64"
[%%expect {|
external to_float : float# -> float = "%box_float"
external to_int32 : int32# -> int32 = "%box_int32"
external to_int64 : int64# -> int64 = "%box_int64"
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
module M1 : sig val poly_ fst : ('a : float64). 'a -> 'b -> 'a end
- : float * float = (3.14, 3.14)
|}];;

module M2 : sig
  val poly_ fst : ('b : bits32). 'a -> 'b -> 'a
end = Fst;;
(M2.fst 42 #1337l, M2.fst #3.14 #1337l |> to_float)
[%%expect {|
module M2 : sig val poly_ fst : ('b : bits32). 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
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
module M0 :
  sig val fst : layout_ l l0 l1. ('a : l0) ('b : l1). 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}];;

module M1 : sig
  val fst : layout_ x z y. ('a : x) ('b : y). 'a -> 'b -> 'a
end = Fst;;
(M1.fst 42 #1337l, M1.fst #3.14 #1337l |> to_float)
[%%expect {|
module M1 :
  sig val fst : layout_ l l0 l1. ('a : l) ('b : l1). 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}];;

module M2 : sig
  val fst : layout_ x y z. ('a : x) ('b : y). 'a -> 'b -> 'a
end = Fst;;
(M2.fst 42 #1337l, M2.fst #3.14 #1337l |> to_float)
[%%expect {|
module M2 :
  sig val fst : layout_ l l0 l1. ('a : l) ('b : l0). 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}];;

(* eta-expanding and instantiating coercion *)

module M12 : sig
  val fst : layout_ z. ('a : float64) ('b : bits32). 'a -> 'b -> 'a
end = Fst;;
(M12.fst #3.14 #1337l |> to_float)
[%%expect {|
module M12 :
  sig val fst : layout_ l. ('a : float64) ('b : bits32). 'a -> 'b -> 'a end
- : float = 3.14
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
module M23 :
  sig val fst : layout_ l l0 l1 l2. ('a : l) ('b : l0). 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}];;

module M20 = ((Fst : sig
  val fst : layout_ x y z. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : layout_ w x y z. ('a : x) ('b : y). 'a -> 'b -> 'a
end);;
(M20.fst 42 #1337l, M20.fst #3.14 #1337l |> to_float)
[%%expect {|
module M20 :
  sig val fst : layout_ l l0 l1 l2. ('a : l0) ('b : l1). 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}];;

module M02 = ((Fst : sig
  val fst : layout_ w x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : layout_ w x y z. ('a : x) ('b : y). 'a -> 'b -> 'a
end);;
(M02.fst 42 #1337l, M02.fst #3.14 #1337l |> to_float)
[%%expect {|
module M02 :
  sig val fst : layout_ l l0 l1 l2. ('a : l0) ('b : l1). 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}];;

module M00 = ((Fst : sig
  val fst : layout_ z x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : layout_ z w x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end);;
(M00.fst 42 #1337l, M00.fst #3.14 #1337l |> to_float)
[%%expect {|
module M00 :
  sig val fst : layout_ l l0 l1 l2. ('a : l1) ('b : l2). 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}];;

(* chaining eta-expanding and eta-reducing coercions *)

module M2 = ((Fst : sig
  val fst : layout_ x y z. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : layout_ x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end);;
(M2.fst 42 #1337l, M2.fst #3.14 #1337l |> to_float)
[%%expect {|
module M2 : sig val poly_ fst : 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}];;

module M0 = ((Fst : sig
  val fst : layout_ z x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : layout_ x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end);;
(M0.fst 42 #1337l, M0.fst #3.14 #1337l |> to_float)
[%%expect {|
module M0 : sig val poly_ fst : 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
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
module M0 : sig val poly_ fst : 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}];;

module M1 = ((Fst : sig
  val fst : layout_ x z y. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : layout_ x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end);;
(M1.fst 42 #1337l, M1.fst #3.14 #1337l |> to_float)
[%%expect {|
module M1 : sig val poly_ fst : 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}];;

module M2 = ((Fst : sig
  val fst : layout_ x y z. ('a : x) ('b : y). 'a -> 'b -> 'a
end) : sig
  val fst : layout_ x y. ('a : x) ('b : y). 'a -> 'b -> 'a
end);;
(M2.fst 42 #1337l, M2.fst #3.14 #1337l |> to_float)
[%%expect {|
module M2 : sig val poly_ fst : 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
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
val box_triple' : #(float# * int32# * int64#) -> float * int32 * int64 =
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
module M132 :
  sig
    val triple :
      layout_ l l0 l1.
        ('a : l) ('b : l1) ('c : l0). 'a -> 'b -> 'c -> #('a * 'b * 'c)
  end
- : (float * int * int) * (float * int32 * int64) =
((3.14, 1337, 42), (3.14, 1337l, 0L))
|}];;

module M213 : sig
  val triple :
    layout_ x y z. ('a : y) ('b : x) ('c : z). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end = Triple;;
(M213.triple 3.14 1337 42 |> box_triple, M213.triple #3.14 #1337l #0L |> box_triple')
[%%expect {|
module M213 :
  sig
    val triple :
      layout_ l l0 l1.
        ('a : l0) ('b : l) ('c : l1). 'a -> 'b -> 'c -> #('a * 'b * 'c)
  end
- : (float * int * int) * (float * int32 * int64) =
((3.14, 1337, 42), (3.14, 1337l, 0L))
|}];;

module M231 : sig
  val triple :
    layout_ x y z. ('a : y) ('b : z) ('c : x). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end = Triple;;
(M231.triple 3.14 1337 42 |> box_triple, M231.triple #3.14 #1337l #0L |> box_triple')
[%%expect {|
module M231 :
  sig
    val triple :
      layout_ l l0 l1.
        ('a : l0) ('b : l1) ('c : l). 'a -> 'b -> 'c -> #('a * 'b * 'c)
  end
- : (float * int * int) * (float * int32 * int64) =
((3.14, 1337, 42), (3.14, 1337l, 0L))
|}];;

module M312 : sig
  val triple :
    layout_ x y z. ('a : z) ('b : x) ('c : y). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end = Triple;;
(M312.triple 3.14 1337 42 |> box_triple, M312.triple #3.14 #1337l #0L |> box_triple')
[%%expect {|
module M312 :
  sig
    val triple :
      layout_ l l0 l1.
        ('a : l1) ('b : l) ('c : l0). 'a -> 'b -> 'c -> #('a * 'b * 'c)
  end
- : (float * int * int) * (float * int32 * int64) =
((3.14, 1337, 42), (3.14, 1337l, 0L))
|}];;

module M321 : sig
  val triple :
    layout_ x y z. ('a : z) ('b : y) ('c : x). 'a -> 'b -> 'c -> #('a * 'b * 'c)
end = Triple;;
(M321.triple 3.14 1337 42 |> box_triple, M321.triple #3.14 #1337l #0L |> box_triple')
[%%expect {|
module M321 :
  sig
    val triple :
      layout_ l l0 l1.
        ('a : l1) ('b : l0) ('c : l). 'a -> 'b -> 'c -> #('a * 'b * 'c)
  end
- : (float * int * int) * (float * int32 * int64) =
((3.14, 1337, 42), (3.14, 1337l, 0L))
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
module M_132_132 :
  sig
    val triple :
      layout_ l l0 l1.
        ('a : l) ('b : l1) ('c : l0). 'a -> 'b -> 'c -> #('a * 'b * 'c)
  end
- : (float * int * int) * (float * int32 * int64) =
((3.14, 1337, 42), (3.14, 1337l, 0L))
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
module M_321_321 :
  sig
    val triple :
      layout_ l l0 l1.
        ('a : l1) ('b : l0) ('c : l). 'a -> 'b -> 'c -> #('a * 'b * 'c)
  end
- : (float * int * int) * (float * int32 * int64) =
((3.14, 1337, 42), (3.14, 1337l, 0L))
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
module M_312_312 :
  sig
    val triple :
      layout_ l l0 l1.
        ('a : l1) ('b : l) ('c : l0). 'a -> 'b -> 'c -> #('a * 'b * 'c)
  end
- : (float * int * int) * (float * int32 * int64) =
((3.14, 1337, 42), (3.14, 1337l, 0L))
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
module M_312_321 :
  sig
    val triple :
      layout_ l l0 l1.
        ('a : l1) ('b : l0) ('c : l). 'a -> 'b -> 'c -> #('a * 'b * 'c)
  end
- : (float * int * int) * (float * int32 * int64) =
((3.14, 1337, 42), (3.14, 1337l, 0L))
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

(* CR jbachurski: Here and in the later tests, the [moregeneral] check has a bug
   that permits setting weak pattern variables (here, from [@layout_poly]),
   to generic subject variables. This weak variable is later defaulted during
   translation (despite appearing generic) which causes this fatal error. *)
(* layout-poly coercion *)
module M : sig
  val poly_ id : 'a -> 'a
end = Id;;
(M.id 42, M.id #3.14 |> to_float)
[%%expect {|
>> Fatal error: Slambdaident.of_sort_var: not a root
Uncaught exception: Misc.Fatal_error

|}];;

(* eta-expanding coercion *)
module M1 : sig
  val id : layout_ x y. ('a : x). 'a -> 'a
end = Id;;
(M1.id 42, M1.id #3.14 |> to_float)
[%%expect {|
>> Fatal error: Slambdaident.of_sort_var: not a root
Uncaught exception: Misc.Fatal_error

|}];;
module M0 : sig
  val id : layout_ y x. ('a : x). 'a -> 'a
end = Id;;
(M0.id 42, M0.id #3.14 |> to_float)
[%%expect {|
>> Fatal error: Slambdaident.of_sort_var: not a root
Uncaught exception: Misc.Fatal_error

|}];;

(* instantiating and eta-expanding coercion *)
module M : sig
  val id : layout_ x. ('a : bits64). 'a -> 'a
end = Id;;
(M.id #42L |> to_int64)
[%%expect {|
module M : sig val id : layout_ l. ('a : bits64). 'a -> 'a end
- : int64 = 42L
|}];;
module M : sig
  val id : layout_ x y. ('a : bits64). 'a -> 'a
end = Id;;
(M.id #42L |> to_int64)
[%%expect {|
module M : sig val id : layout_ l l0. ('a : bits64). 'a -> 'a end
- : int64 = 42L
|}];;

(* chaining primitive and instantiating coercions *)
module M = ((Id : sig
  val id : layout_ x. ('a : x). 'a -> 'a
end) : sig
  val id : ('a : bits64). 'a -> 'a
end);;
(M.id #42L |> to_int64)
[%%expect {|
module M : sig val id : ('a : bits64). 'a -> 'a end
- : int64 = 42L
|}];;

(* chaining non-polymorphic primitive and eta-expanding coercions *)
module M = ((Id : sig
  val id : ('a : bits64). 'a -> 'a
end) : sig
  val id : layout_ x. ('a : bits64). 'a -> 'a
end);;
(M.id #42L |> to_int64)
[%%expect {|
module M : sig val id : layout_ l. ('a : bits64). 'a -> 'a end
- : int64 = 42L
|}];;

(* chaining primitive, eta-expanding and eta-reducing coercions *)
module M0 = ((Id : sig
  val id : layout_ y x. ('a : x). 'a -> 'a
end) : sig
  val id : layout_ x. ('a : x). 'a -> 'a
end);;
(M0.id 42, M0.id #3.14 |> to_float)
[%%expect {|
module M0 : sig val poly_ id : 'a -> 'a end
- : int * float = (42, 3.14)
|}];;

module M1 = ((Id : sig
  val id : layout_ x y. ('a : x). 'a -> 'a
end) : sig
  val id : layout_ x. ('a : x). 'a -> 'a
end);;
(M1.id 42, M1.id #3.14 |> to_float)
[%%expect {|
module M1 : sig val poly_ id : 'a -> 'a end
- : int * float = (42, 3.14)
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
Error: The module is "dynamic"
       but is expected to be "static"
         because it is layout-polymorphic and being instantiated here.
|}];;

(* CR jbachurski: This needs translation of static functors to work,
   but importantly type-checking succeeds. *)
module F_inst_static (M : sig
  val poly_ id : 'a -> 'a
end @ static) : sig
  val id : 'a -> 'a
end = M
[%%expect {|
>> Fatal error: slambda eval: unexpected missing value
Uncaught exception: Misc.Fatal_error

|}];;

module F_eta_dynamic (M : sig
  val id : 'a -> 'a
end @ dynamic) : sig
  val id : layout_ x. 'a -> 'a
end = M
[%%expect {|
module F_eta_dynamic :
  functor (M : sig val id : 'a -> 'a end) ->
    sig val id : layout_ l. 'a -> 'a end
|}];;

module F_eta_static (M : sig
  val id : 'a -> 'a
end @ static) : sig
  val id : layout_ x. 'a -> 'a
end = M
[%%expect {|
module F_eta_static :
  functor (M : sig val id : 'a -> 'a end @ static) ->
    sig val id : layout_ l. 'a -> 'a end
|}];;

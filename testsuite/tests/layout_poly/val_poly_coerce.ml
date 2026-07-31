(* TEST
 flags = "-extension layout_poly_alpha";
 expect.opt;
*)

external to_float : float# -> float = "%box_float"
[%%expect {|
external to_float : float# -> float = "%box_float"
|}];;

(* This test file requires both layout-polymorphic let and value items to work
   (tested by [let_poly.ml] and [val_poly.ml] respectively). *)

(** coercions on the [id]entity function **)

module Id = struct
  let poly_ id x = x
end;;
(Id.id 42, Id.id #3.14 |> to_float)
[%%expect {|
module Id : sig val poly_ id : 'a -> 'a end
>> Fatal error: slambda eval: unexpected missing value
Uncaught exception: Misc.Fatal_error

|}];;

(* no coercion *)
module M : sig
  val poly_ id : 'a -> 'a
end = Id;;
(M.id 42, M.id #3.14 |> to_float)
[%%expect {|
module M : sig val poly_ id : 'a -> 'a end
>> Fatal error: slambda eval: unexpected missing value
Uncaught exception: Misc.Fatal_error

|}];;

(* instantiating coercion *)
module M : sig
  val id : ('a : float64). 'a -> 'a
end = Id;;
(M.id #3.14 |> to_float)
[%%expect {|
Line 3, characters 6-8:
3 | end = Id;;
          ^^
Error: Signature mismatch:
       Modules do not match:
         sig val poly_ id : 'a -> 'a end
       is not included in
         sig val id : ('a : float64). 'a -> 'a end
       Values do not match:
         val poly_ id : 'a -> 'a
       is not included in
         val id : ('a : float64). 'a -> 'a
       the first has 1 more layout parameter that is not used,
       which is not supported yet.
|}];;

(* eta-expanding coercion *)
module M : sig
  val id : layout_ x y. ('a : x). 'a -> 'a
end = Id;;
(M.id 42, M.id #3.14 |> to_float)
[%%expect {|
Line 3, characters 6-8:
3 | end = Id;;
          ^^
Error: Signature mismatch:
       Modules do not match:
         sig val poly_ id : 'a -> 'a end
       is not included in
         sig val id : layout_ l l0. ('a : l). 'a -> 'a end
       Values do not match:
         val poly_ id : 'a -> 'a
       is not included in
         val id : layout_ l l0. ('a : l). 'a -> 'a
       the second has 1 more layout parameter that is not used,
       which is not supported yet.
|}];;
module M : sig
  val id : layout_ y x. ('a : x). 'a -> 'a
end = Id;;
(M.id 42, M.id #3.14 |> to_float)
[%%expect {|
Line 3, characters 6-8:
3 | end = Id;;
          ^^
Error: Signature mismatch:
       Modules do not match:
         sig val poly_ id : 'a -> 'a end
       is not included in
         sig val id : layout_ l l0. ('a : l0). 'a -> 'a end
       Values do not match:
         val poly_ id : 'a -> 'a
       is not included in
         val id : layout_ l l0. ('a : l0). 'a -> 'a
       The layout parameter at position 1 in the first
       corresponds to the parameter at position 2 in the second,
       which is not supported yet.
|}];;

(* instantiating and eta-expanding coercion *)
module M : sig
  val id : layout_ x. ('a : bits64). 'a -> 'a
end = Id;;
(M.id #3.14 |> to_float)
[%%expect {|
Line 3, characters 6-8:
3 | end = Id;;
          ^^
Error: Signature mismatch:
       Modules do not match:
         sig val poly_ id : 'a -> 'a end
       is not included in
         sig val id : layout_ l. ('a : bits64). 'a -> 'a end
       Values do not match:
         val poly_ id : 'a -> 'a
       is not included in
         val id : layout_ l. ('a : bits64). 'a -> 'a
       The layout parameter at position 1 in the first
       is instantiated with layout "bits64",
       which is not supported yet.
|}];;
module M : sig
  val id : layout_ x y. ('a : bits64). 'a -> 'a
end = Id;;
(M.id #3.14 |> to_float)
[%%expect {|
Line 3, characters 6-8:
3 | end = Id;;
          ^^
Error: Signature mismatch:
       Modules do not match:
         sig val poly_ id : 'a -> 'a end
       is not included in
         sig val id : layout_ l l0. ('a : bits64). 'a -> 'a end
       Values do not match:
         val poly_ id : 'a -> 'a
       is not included in
         val id : layout_ l l0. ('a : bits64). 'a -> 'a
       The layout parameter at position 1 in the first
       is instantiated with layout "bits64",
       which is not supported yet.
|}];;


(** coercions on the [cond]itional function **)

module Cond = struct
  let poly_ cond b x y = if b then x else y
end;;
[%%expect {|
module Cond : sig val poly_ cond : bool -> 'a -> 'a -> 'a end
|}];;
